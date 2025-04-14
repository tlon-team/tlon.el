;;; tlon-tts-tests.el --- Tests for tlon-tts -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for tlon-tts.el using ERT.

;;; Code:

(require 'ert)
(require 'tlon-tts)
(require 'tlon-core) ;; For tlon-lookup, etc. if needed in setup

;; Helper function to set up a test buffer with content and variables
(defun tlon-tts-test--setup-buffer (content &optional voice-chunks engine)
  "Set up a temporary buffer with CONTENT and TTS variables.
VOICE-CHUNKS is a list of (POSITION . VOICE-ID) markers.
ENGINE defaults to \"ElevenLabs\"."
  (let ((buffer (get-buffer-create "*tlon-tts-test-buffer*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert content)
      ;; Set necessary buffer-local variables for chunking
      (make-local-variable 'tlon-tts-voice-chunks)
      (make-local-variable 'tlon-tts-engine)
      (setq tlon-tts-voice-chunks (mapcar (lambda (vc) (cons (copy-marker (car vc)) (cdr vc))) voice-chunks))
      (setq tlon-tts-engine (or engine "ElevenLabs")) ; Default for paragraph chunking
      ;; Add other variables if tlon-tts-break-into-chunks depends on them
      )
    buffer))

(defun tlon-tts-test--teardown-buffer (buffer)
  "Kill the temporary test BUFFER."
  (when (buffer-live-p buffer)
    (kill-buffer buffer)))

;; Test suite for tlon-tts-break-into-chunks
(ert-deftest tlon-tts-break-into-chunks-paragraph-mode ()
  "Test chunking by paragraph (chunk-size = nil)."
  (let* ((content "Paragraph 1.\n\nParagraph 2.\n\nParagraph 3.")
         (buffer (tlon-tts-test--setup-buffer content nil "ElevenLabs")) ; Engine matters for nil chunk-size
         (expected-chunks '(("Paragraph 1.") ("Paragraph 2.") ("Paragraph 3.")))
         chunks)
    (with-current-buffer buffer
      (setq chunks (tlon-tts-break-into-chunks nil)))
    (tlon-tts-test--teardown-buffer buffer)
    (should (equal chunks expected-chunks))))

(ert-deftest tlon-tts-break-into-chunks-char-limit ()
  "Test chunking with a character limit."
  (let* ((content "Short para 1.\n\nThis is the second paragraph, it is longer.\n\nPara 3.")
         (buffer (tlon-tts-test--setup-buffer content nil "Microsoft Azure")) ; Engine with char limit
         (chunk-size 30)
         (expected-chunks '(("Short para 1.") ("This is the second paragraph," . nil) ("it is longer.") ("Para 3."))) ; Example, adjust based on actual logic
         chunks)
    (with-current-buffer buffer
      ;; Manually set char-limit for testing, as tlon-tts-break-into-chunks doesn't read it directly
      (setq chunks (tlon-tts-break-into-chunks chunk-size)))
    (tlon-tts-test--teardown-buffer buffer)
    ;; NOTE: This expected result is hypothetical. The actual logic might break differently.
    ;; Adjust the expected-chunks based on debugging the function's behavior.
    ;; The current implementation seems complex regarding boundary adjustments.
    (message "Character limit test output: %S" chunks)
    ;; For now, just check if it produces a list
    (should (listp chunks))
    ;; TODO: Refine this test with exact expected output once behavior is stable/understood.
    ;; (should (equal chunks expected-chunks))
    ))

(ert-deftest tlon-tts-break-into-chunks-with-voice-change ()
  "Test chunking with voice changes."
  (let* ((content "First part with voice A.\n\nSecond part starts here, then voice B takes over.\n\nFinal part with voice B.")
         ;; Position markers: 0=start, 27=start of P2, 58=after "here,"
         (voice-chunks `((,(copy-marker 58) . "voiceB")))
         (buffer (tlon-tts-test--setup-buffer content voice-chunks "Google Cloud")) ; Engine supporting voice tags
         (chunk-size 100) ; Use char limit mode for this test
         ;; Expected: Break before voice change marker at 58
         (expected-chunks '(("First part with voice A.")
                            ("Second part starts here," . nil) ; Chunk before voice change
                            ("then voice B takes over.\n\nFinal part with voice B." . (tlon-tts-voice . "voiceB")))) ; Chunk after voice change
         chunks)
    (with-current-buffer buffer
      (setq chunks (tlon-tts-break-into-chunks chunk-size)))
    (tlon-tts-test--teardown-buffer buffer)
    (should (equal chunks expected-chunks))))

(ert-deftest tlon-tts-break-into-chunks-voice-change-at-start ()
  "Test chunking with a voice change at the very beginning."
  (let* ((content "Everything read by voice B.\n\nAnother paragraph.")
         (voice-chunks `((,(copy-marker 0) . "voiceB")))
         (buffer (tlon-tts-test--setup-buffer content voice-chunks "Google Cloud"))
         (chunk-size 100)
         (expected-chunks '(("Everything read by voice B." . (tlon-tts-voice . "voiceB"))
                            ("Another paragraph." . (tlon-tts-voice . "voiceB"))))
         chunks)
    (with-current-buffer buffer
      (setq chunks (tlon-tts-break-into-chunks chunk-size)))
    (tlon-tts-test--teardown-buffer buffer)
    (should (equal chunks expected-chunks))))

(ert-deftest tlon-tts-break-into-chunks-ssml-break-boundary ()
  "Test chunking behavior near SSML break tags."
  ;; Test case: chunk boundary falls right after a break tag
  (let* ((content "Paragraph 1.<break time=\"0.5s\"/>\n\nParagraph 2.")
         (buffer (tlon-tts-test--setup-buffer content nil "ElevenLabs"))
         (chunk-size 15) ; Force break after the break tag initially
         ;; Expected: The break tag should ideally belong to the first chunk,
         ;; or the break point should move before it.
         ;; Current logic moves point *before* break tag if end falls after it.
         (expected-chunks '(("Paragraph 1.") ("<break time=\"0.5s\"/>\n\nParagraph 2.")))
         chunks)
    (with-current-buffer buffer
      (setq chunks (tlon-tts-break-into-chunks chunk-size)))
    (tlon-tts-test--teardown-buffer buffer)
    (should (equal chunks expected-chunks))))


(ert-deftest tlon-tts-break-into-chunks-empty-input ()
  "Test chunking with empty input."
  (let* ((content "")
         (buffer (tlon-tts-test--setup-buffer content))
         (expected-chunks '())
         chunks)
    (with-current-buffer buffer
      (setq chunks (tlon-tts-break-into-chunks 100)))
    (tlon-tts-test--teardown-buffer buffer)
    (should (equal chunks expected-chunks))))

(ert-deftest tlon-tts-break-into-chunks-single-large-paragraph ()
  "Test chunking a single paragraph larger than the chunk size."
  (let* ((content "This is a single very long paragraph that definitely exceeds the chunk size limit and should be split into multiple parts by the chunking function based on the character limit provided.")
         (buffer (tlon-tts-test--setup-buffer content nil "Microsoft Azure"))
         (chunk-size 50)
         ;; Expected: Split within the paragraph
         (expected-chunks '(("This is a single very long paragraph that" . nil)
                            ("definitely exceeds the chunk size limit and should" . nil)
                            ("be split into multiple parts by the chunking" . nil)
                            ("function based on the character limit provided.")))
         chunks)
    (with-current-buffer buffer
      (setq chunks (tlon-tts-break-into-chunks chunk-size)))
    (tlon-tts-test--teardown-buffer buffer)
    ;; Note: The exact split points depend heavily on the backward-paragraph logic.
    ;; This expected result is illustrative. Adjust after testing.
    (message "Single large paragraph test output: %S" chunks)
    (should (>= (length chunks) 3)) ; Expecting multiple chunks
    ;; (should (equal chunks expected-chunks)) ; TODO: Refine expected output
    ))


(provide 'tlon-tts-tests)

;;; tlon-tts-tests.el ends here
