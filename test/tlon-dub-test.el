;;; tlon-dub-test.el --- Tests for tlon-dub -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for dubbing helpers: timestamp conversion, speaker extraction,
;; content type detection, VTT/SRT parsing, CSV escaping, and text cleaning.

;;; Code:

(require 'ert)
(require 'tlon-dub)

;;;; tlon-dub--timestamp-to-seconds

(ert-deftest tlon-dub-timestamp-to-seconds-basic ()
  "Convert simple SRT timestamp."
  (should (= 5.5 (tlon-dub--timestamp-to-seconds "00:00:05,500"))))

(ert-deftest tlon-dub-timestamp-to-seconds-hours ()
  "Convert timestamp with hours."
  (should (= 3750.25 (tlon-dub--timestamp-to-seconds "01:02:30,250"))))

(ert-deftest tlon-dub-timestamp-to-seconds-zero ()
  "Zero timestamp."
  (should (= 0.0 (tlon-dub--timestamp-to-seconds "00:00:00,000"))))

(ert-deftest tlon-dub-timestamp-to-seconds-invalid ()
  "Return nil for invalid format."
  (should (null (tlon-dub--timestamp-to-seconds "invalid"))))

;;;; tlon-dub--dot-timestamp-to-seconds

(ert-deftest tlon-dub-dot-timestamp-to-seconds-basic ()
  "Convert dot-format timestamp."
  (should (= 5.5 (tlon-dub--dot-timestamp-to-seconds "0:00:05.500"))))

(ert-deftest tlon-dub-dot-timestamp-to-seconds-hours ()
  "Convert timestamp with hours."
  (should (= 5025.25 (tlon-dub--dot-timestamp-to-seconds "1:23:45.250"))))

(ert-deftest tlon-dub-dot-timestamp-to-seconds-invalid ()
  "Return nil for invalid format."
  (should (null (tlon-dub--dot-timestamp-to-seconds "not a timestamp"))))

;;;; tlon-dub--seconds-to-dot-timestamp

(ert-deftest tlon-dub-seconds-to-dot-timestamp-basic ()
  "Convert seconds to dot-format timestamp."
  (should (equal "00:00:05.500" (tlon-dub--seconds-to-dot-timestamp 5.5))))

(ert-deftest tlon-dub-seconds-to-dot-timestamp-hours ()
  "Convert with hours."
  (should (equal "01:01:01.250" (tlon-dub--seconds-to-dot-timestamp 3661.25))))

(ert-deftest tlon-dub-seconds-to-dot-timestamp-zero ()
  "Zero seconds."
  (should (equal "00:00:00.000" (tlon-dub--seconds-to-dot-timestamp 0.0))))

(ert-deftest tlon-dub-seconds-to-dot-timestamp-round-trip ()
  "Round-trip: seconds -> timestamp -> seconds."
  (should (= 5025.5
             (tlon-dub--dot-timestamp-to-seconds
              (tlon-dub--seconds-to-dot-timestamp 5025.5)))))

;;;; tlon-dub--get-speaker

(ert-deftest tlon-dub-get-speaker-single-name ()
  "Single-word name doesn't match (regex requires 2+ capitalized words)."
  (should (null (tlon-dub--get-speaker "Alice: Hello"))))

(ert-deftest tlon-dub-get-speaker-two-names ()
  "Extract two-word speaker."
  (should (equal "John Doe:" (tlon-dub--get-speaker "John Doe: Hello"))))

(ert-deftest tlon-dub-get-speaker-three-names ()
  "Extract three-word speaker."
  (should (equal "Rob Wiblin Jr:" (tlon-dub--get-speaker "Rob Wiblin Jr: Hello"))))

(ert-deftest tlon-dub-get-speaker-no-prefix ()
  "Return nil when no speaker prefix."
  (should (null (tlon-dub--get-speaker "just some text"))))

(ert-deftest tlon-dub-get-speaker-lowercase ()
  "Return nil for lowercase names."
  (should (null (tlon-dub--get-speaker "john doe: hello"))))

;;;; tlon-dub--get-content-type

(ert-deftest tlon-dub-content-type-mp3 ()
  "Detect MP3 content type."
  (should (equal "audio/mpeg" (tlon-dub--get-content-type "audio.mp3"))))

(ert-deftest tlon-dub-content-type-mp4 ()
  "Detect MP4 content type."
  (should (equal "video/mp4" (tlon-dub--get-content-type "video.mp4"))))

(ert-deftest tlon-dub-content-type-case-insensitive ()
  "Extension matching is case-insensitive."
  (should (equal "video/mp4" (tlon-dub--get-content-type "video.MP4"))))

(ert-deftest tlon-dub-content-type-wav ()
  "Detect WAV content type."
  (should (equal "audio/wav" (tlon-dub--get-content-type "audio.wav"))))

(ert-deftest tlon-dub-content-type-unknown ()
  "Return nil for unknown extension."
  (should (null (tlon-dub--get-content-type "file.xyz"))))

;;;; tlon-dub--ends-sentence-p

(ert-deftest tlon-dub-ends-sentence-period ()
  "Period ends sentence."
  (should (tlon-dub--ends-sentence-p "Hello world.")))

(ert-deftest tlon-dub-ends-sentence-exclamation ()
  "Exclamation mark ends sentence."
  (should (tlon-dub--ends-sentence-p "Hello!")))

(ert-deftest tlon-dub-ends-sentence-question ()
  "Question mark ends sentence."
  (should (tlon-dub--ends-sentence-p "What?")))

(ert-deftest tlon-dub-ends-sentence-ellipsis ()
  "Ellipsis ends sentence."
  (should (tlon-dub--ends-sentence-p "And then…")))

(ert-deftest tlon-dub-ends-sentence-no ()
  "Text without punctuation does not end sentence."
  (should-not (tlon-dub--ends-sentence-p "Hello world")))

;;;; tlon-dub--clean-segment-text

(ert-deftest tlon-dub-clean-segment-text-removes-speaker ()
  "Remove speaker prefix (requires 2+ capitalized words)."
  (should (equal "Hello world" (tlon-dub--clean-segment-text "Rob Wiblin: Hello world"))))

(ert-deftest tlon-dub-clean-segment-text-collapses-whitespace ()
  "Collapse multiple spaces and newlines."
  (should (equal "Hello world" (tlon-dub--clean-segment-text "Hello   world"))))

(ert-deftest tlon-dub-clean-segment-text-no-speaker ()
  "Text without speaker returned cleaned."
  (should (equal "Normal text" (tlon-dub--clean-segment-text "Normal text"))))

;;;; tlon-dub--csv-escape-string

(ert-deftest tlon-dub-csv-escape-basic ()
  "Wrap in quotes."
  (should (equal "\"hello\"" (tlon-dub--csv-escape-string "hello"))))

(ert-deftest tlon-dub-csv-escape-quotes ()
  "Double internal quotes."
  (should (equal "\"say \"\"hi\"\"\"" (tlon-dub--csv-escape-string "say \"hi\""))))

;;;; tlon-dub--make-csv-line

(ert-deftest tlon-dub-make-csv-line-basic ()
  "Format a CSV line with all fields."
  (let ((line (tlon-dub--make-csv-line "Alice" "00:00:00,000" "00:00:05,000" "Hello" "Hola")))
    (should (string-match-p "Alice" line))
    (should (string-match-p "Hello" line))
    (should (string-match-p "Hola" line))))

(ert-deftest tlon-dub-make-csv-line-nil-speaker ()
  "Nil speaker becomes empty string."
  (let ((line (tlon-dub--make-csv-line nil "0:00" "0:05" "Hi" "Hola")))
    (should (string-prefix-p "\"\"," line))))

;;;; tlon-dub--parse-vtt

(ert-deftest tlon-dub-parse-vtt-basic ()
  "Parse simple VTT with two segments."
  (let ((vtt "WEBVTT\n\n00:00:01.000 --> 00:00:03.000\nHello\n\n00:00:04.000 --> 00:00:06.000\nWorld"))
    (let ((segments (tlon-dub--parse-vtt vtt)))
      (should (= 2 (length segments)))
      (should (equal "Hello" (plist-get (car segments) :text)))
      (should (equal "World" (plist-get (cadr segments) :text))))))

(ert-deftest tlon-dub-parse-vtt-skips-notes ()
  "Skip NOTE comments."
  (let ((vtt "WEBVTT\n\nNOTE This is a comment\n\n00:00:01.000 --> 00:00:03.000\nText"))
    (let ((segments (tlon-dub--parse-vtt vtt)))
      (should (= 1 (length segments))))))

(ert-deftest tlon-dub-parse-vtt-empty ()
  "Empty VTT returns nil."
  (should (null (tlon-dub--parse-vtt "WEBVTT\n\n"))))

;;;; tlon-dub--parse-srt

(ert-deftest tlon-dub-parse-srt-basic ()
  "Parse simple SRT file."
  (let ((file (make-temp-file "tlon-srt-" nil ".srt")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "1\n00:00:00,000 --> 00:00:05,000\nHello world\n\n2\n00:00:06,000 --> 00:00:10,000\nGoodbye\n"))
          (let ((segments (tlon-dub--parse-srt file)))
            (should (= 2 (length segments)))
            (should (equal "Hello world" (plist-get (car segments) :text)))))
      (delete-file file))))

(provide 'tlon-dub-test)
;;; tlon-dub-test.el ends here
