;;; tlon-tts-test.el --- Tests for tlon-tts -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for TTS text processing functions: XML escaping, chunk naming,
;; hash table merging, voice state management, and verbal exponents.

;;; Code:

(require 'ert)
(require 'tlon-tts)

;;;; tlon-tts-escape-xml-special-characters-in-text

(ert-deftest tlon-tts-escape-xml-ampersand ()
  "Escape ampersand."
  (should (equal "&amp;" (tlon-tts-escape-xml-special-characters-in-text "&"))))

(ert-deftest tlon-tts-escape-xml-angle-brackets ()
  "Escape angle brackets."
  (should (equal "&lt;tag&gt;"
                 (tlon-tts-escape-xml-special-characters-in-text "<tag>"))))

(ert-deftest tlon-tts-escape-xml-quotes ()
  "Escape double and single quotes."
  (should (equal "&quot;hello&apos;"
                 (tlon-tts-escape-xml-special-characters-in-text "\"hello'"))))

(ert-deftest tlon-tts-escape-xml-mixed ()
  "Escape a string with multiple special characters."
  (should (equal "A &amp; B &lt; C"
                 (tlon-tts-escape-xml-special-characters-in-text "A & B < C"))))

(ert-deftest tlon-tts-escape-xml-no-specials ()
  "Return unchanged string with no special characters."
  (should (equal "plain text" (tlon-tts-escape-xml-special-characters-in-text "plain text"))))

;;;; tlon-tts-get-chunk-name

(ert-deftest tlon-tts-get-chunk-name-basic ()
  "Generate chunk filename with zero-padded number."
  (should (equal "/path/audio-chunk-001.mp3"
                 (tlon-tts-get-chunk-name "/path/audio.mp3" 1))))

(ert-deftest tlon-tts-get-chunk-name-large-number ()
  "Handle large chunk numbers."
  (should (equal "/path/audio-chunk-042.mp3"
                 (tlon-tts-get-chunk-name "/path/audio.mp3" 42))))

(ert-deftest tlon-tts-get-chunk-name-different-extension ()
  "Preserve the original file extension."
  (should (equal "/path/speech-chunk-003.wav"
                 (tlon-tts-get-chunk-name "/path/speech.wav" 3))))

;;;; tlon-tts-get-original-filename

(ert-deftest tlon-tts-get-original-filename-basic ()
  "Remove chunk suffix to recover original filename."
  (should (equal "/path/audio.mp3"
                 (tlon-tts-get-original-filename "/path/audio-chunk-001.mp3"))))

(ert-deftest tlon-tts-get-original-filename-no-chunk ()
  "Return unchanged if no chunk suffix present."
  (should (equal "/path/audio.mp3"
                 (tlon-tts-get-original-filename "/path/audio.mp3"))))

(ert-deftest tlon-tts-get-original-filename-round-trip ()
  "Round-trip: chunk name -> original should yield original."
  (let ((original "/path/speech.wav"))
    (should (equal original
                   (tlon-tts-get-original-filename
                    (tlon-tts-get-chunk-name original 5))))))

;;;; tlon-tts--hash-table-merge

(ert-deftest tlon-tts-hash-table-merge-basic ()
  "Merge two hash tables."
  (let ((base (make-hash-table :test 'equal))
        (overrides (make-hash-table :test 'equal)))
    (puthash "a" 1 base)
    (puthash "b" 2 base)
    (puthash "b" 20 overrides)
    (puthash "c" 3 overrides)
    (let ((result (tlon-tts--hash-table-merge base overrides)))
      (should (equal 1 (gethash "a" result)))
      (should (equal 20 (gethash "b" result)))
      (should (equal 3 (gethash "c" result))))))

(ert-deftest tlon-tts-hash-table-merge-nil-base ()
  "Merge with nil base."
  (let ((overrides (make-hash-table :test 'equal)))
    (puthash "x" 1 overrides)
    (let ((result (tlon-tts--hash-table-merge nil overrides)))
      (should (equal 1 (gethash "x" result))))))

(ert-deftest tlon-tts-hash-table-merge-nil-overrides ()
  "Merge with nil overrides."
  (let ((base (make-hash-table :test 'equal)))
    (puthash "x" 1 base)
    (let ((result (tlon-tts--hash-table-merge base nil)))
      (should (equal 1 (gethash "x" result))))))

(ert-deftest tlon-tts-hash-table-merge-both-nil ()
  "Merge two nil arguments returns empty hash table."
  (let ((result (tlon-tts--hash-table-merge nil nil)))
    (should (hash-table-p result))
    (should (= 0 (hash-table-count result)))))

;;;; tlon-tts--update-voice-state

(ert-deftest tlon-tts-update-voice-state-at-change-point ()
  "When begin equals the voice change position, switch voice."
  (let ((result (tlon-tts--update-voice-state 100 100 "new-voice" "old-voice"
                                               '((100 . "new-voice") (200 . "other")))))
    (should (equal "new-voice" (car result)))
    (should (equal '((200 . "other")) (cdr result)))))

(ert-deftest tlon-tts-update-voice-state-not-at-change-point ()
  "When begin is not at the voice change position, keep current voice."
  (let ((result (tlon-tts--update-voice-state 50 100 "new-voice" "old-voice"
                                               '((100 . "new-voice")))))
    (should (equal "old-voice" (car result)))
    (should (equal '((100 . "new-voice")) (cdr result)))))

;;;; tlon-tts--parse-elevenlabs-request-id

(ert-deftest tlon-tts-parse-elevenlabs-request-id-found ()
  "Extract request-id from curl headers."
  (let ((output "HTTP/2 200\ncontent-type: audio/mpeg\nrequest-id: abc-123-def\n\n"))
    (should (equal "abc-123-def"
                   (tlon-tts--parse-elevenlabs-request-id output)))))

(ert-deftest tlon-tts-parse-elevenlabs-request-id-not-found ()
  "Return nil when no request-id header."
  (let ((output "HTTP/2 200\ncontent-type: audio/mpeg\n\n"))
    (should (null (tlon-tts--parse-elevenlabs-request-id output)))))

;;;; tlon-tts-get-irregular-verbal-exponent

(ert-deftest tlon-tts-get-irregular-verbal-exponent-known ()
  "Return the irregular verbal form when available."
  (when (and (boundp 'tlon-tts-irregular-exponents)
             tlon-tts-irregular-exponents)
    (let* ((first-entry (car tlon-tts-irregular-exponents))
           (exponent (car first-entry))
           (inner (cdr first-entry))
           (lang (caar inner))
           (expected (cdar inner)))
      (should (equal expected
                     (tlon-tts-get-irregular-verbal-exponent exponent lang))))))

(ert-deftest tlon-tts-get-irregular-verbal-exponent-unknown ()
  "Return nil for an unknown exponent."
  (should (null (tlon-tts-get-irregular-verbal-exponent "99999" "es"))))

;;;; tlon-tts-get-regular-verbal-exponent

(ert-deftest tlon-tts-get-regular-verbal-exponent-basic ()
  "Format exponent using the regular pattern."
  (when (and (boundp 'tlon-tts-regular-exponent-pattern)
             tlon-tts-regular-exponent-pattern)
    (let* ((lang (caar tlon-tts-regular-exponent-pattern))
           (pattern (cdar tlon-tts-regular-exponent-pattern)))
      (should (equal (format pattern "5")
                     (tlon-tts-get-regular-verbal-exponent "5" lang))))))

;;;; tlon-tts-enclose-in-cue-delimiter

(ert-deftest tlon-tts-enclose-in-cue-delimiter-basic ()
  "Enclose string in cue delimiters."
  (let ((tlon-tts-cue-delimiter "***"))
    (should (equal "***hello***\n"
                   (tlon-tts-enclose-in-cue-delimiter "hello")))))

(provide 'tlon-tts-test)
;;; tlon-tts-test.el ends here
