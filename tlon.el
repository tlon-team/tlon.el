;;; tlon.el --- A collection of convenience functions to be used by the Tlön team. -*- lexical-binding: t -*-

;; Author: Pablo Stafforini
;; Maintainer: Pablo Stafforini
;; Version: 0.1.0
;; Package-Requires: ((org "9.1") (ox-hugo "1.2.0"))
;; Homepage: https://tlon.team
;; Keywords: convenience tools


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;;; Code:

(defun ps/tlon-meeting-with (person tareas-id meetings-id pending-id)
  "TODO: docstring"
  ;; "[person] > Meetings > Pending for next meeting" org heading in `work-dashboard.org'
  (ps/org-id-goto pending-id)
  (let ((contents (ps/org-get-heading-contents)))
    (ps/org-clear-heading-contents)
    ;; "[person] > Meetings" org heading in `work-dashboard.org'
    (ps/org-id-goto meetings-id)
    (org-narrow-to-subtree)
    (goto-char (point-max))
    (org-insert-heading)
    (insert "Meeting on ")
    (ps/org-time-stamp-inactive-current-time)
    (unless (string= contents "")
      (insert (concat "\nTo discuss:\n" contents "\n"))))
  (forward-line)
  (ps/org-narrow-to-entry-and-children)
  (ps/window-split-if-unsplit)
  (ps/switch-to-last-window)
  ;; "Tareas" org heading in `tareas.org'
  (ps/org-id-goto tareas-id)
  (ps/org-narrow-to-entry-and-children)
  (ps/switch-to-last-window))

(defun ps/tlon-bae-format-file (&optional extension)
  "Return a file name based on user supplied information.
If EXTENSION is not provided, markdown is used."
  (let* ((lastname (read-string "Last name(s) [separated by spaces if more than one author]: "))
	 (title (read-string "Title: "))
	 (slug-lastname (ps/bibtex-asciify-string (org-hugo-slug lastname)))
	 (slug-title (ps/bibtex-asciify-string (org-hugo-slug title)))
	 (extension (or extension "md")))
    (file-name-with-extension (concat slug-lastname "--" slug-title) extension)))

(defun ps/tlon-bae-rename-file (&optional extension)
  "Rename file at point based on user-supplied information.
If EXTENSION is not provided, markdown is used."
  (interactive)
  (let* ((source-file-path (dired-get-filename)))
    (rename-file
     source-file-path
     (file-name-concat
      (file-name-directory source-file-path)
      (ps/tlon-bae-format-file extension))))
  (revert-buffer))

(defun ps/tlon-bae-create-file (&optional extension)
  "Create a new file based on user-supplied information.
Prompt the user for bibliographic information and create a new
 file based on it in the current directory. If EXTENSION is not
 provided, markdown is used."
  (interactive)
  (find-file (ps/tlon-bae-format-file extension)))

(defvar ps/tlon-markdown-eawiki-footnote-source
  "\\[\\^\\[\\\\\\[\\([[:digit:]]\\{1,2\\}\\)\\\\\\]\\](#.+?)\\^\\]{#.+? .footnote-reference role=\"doc-noteref\"}"
  "Regexp to match footnotes in the main body.")

(defvar ps/tlon-markdown-eawiki-footnote-source2
  "\\[\\^\\[\\\\\\[\\([[:digit:]]\\)\\\\\\]\\](#.+?)\\^\\]{#\\\\\\\\\\\\\".+?\\\\\\\\\\\\\" .\\\\\\\\\\\\\\\"footnote-reference\\\\\\\\\\\\\" role=\"\\\\\\\\\\\\\"doc-noteref\\\\\\\\\\\\\"\"}"
  "Regexp to match footnotes in the main body.")

(defvar ps/tlon-markdown-eawiki-footnote-target
  "\\([[:digit:]]\\{1,2\\}\\). *?\\[\\[^\\*\\*\\[\\\\^\\](#.+?)\\*\\*^\\]{\\.footnote-back-link}\\]{#.+?}

    ::: footnote-content"

  "Regexp to match footnotes in the footnote section.")

(defvar ps/tlon-markdown-eawiki-footnote-target2
  "\\([[:digit:]]\\{1,2\\}\\)\\. *?\\[\\[^\\*\\*\\[\\\\^\\](#.+?)\\*\\*^\\]{\..+?}\\]{#.+?}

    ::: footnote-content "
  "Regexp to match footnotes in the footnote section.")

(defvar ps/tlon-markdown-eawiki-footnote-target3
  "\\([[:digit:]]\\{1,2\\}\\)\\. *?\\[\\[\\^\\*\\*\\[\\\\\\^\\](\\\\%22#.+?\\\\%22)\\*\\*\\^\\]{\\.\\\\\\\\\\\\\"footnote-back-link\\\\\\\\\\\\\"}\\]{#\\\\\\\\\\\\\\\".+?\\\\\\\\\\\\\\\"}

    ::: \\\\\"footnote-content\\\\\" "
  "Regexp to match footnotes in the footnote section.")

(defvar ps/tlon-markdown-eawiki-footnote-target4
  "\\([[:digit:]]\\{1,2\\}\\)\\. *?\\[\\[^\\*\\*\\[\\\\^\\](.+?)\\*\\*^\\]\\]{#.+?}

    footnote-content "
  "Regexp to match footnotes in the footnote section.")

(defvar ps/tlon-markdown-eawiki-footnote-target5
  "\\([[:digit:]]\\{1,2\\}\\)\\. *?\\[\\[^\\*\\*\\[\\\\^\\](#.+?)\\*\\*^\\]\\]{#.+?}

    footnote-content "
  "Regexp to match footnotes in the footnote section.")

(defvar ps/tlon-markdown-eawiki-links
  "\\[\\(.+?\\)\\](\\\\%22\\(.+?\\)\\\\%22)"
  "Regexp to match links.")

(defvar ps/tlon-markdown-eawiki-escaped-quotes
  "\\\\\\\\\\\\\""
  "Regexp to match escaped quotes.")

(defun ps/tlon-markdown-eawiki-cleanup (&optional buffer)
  "Cleanup the buffer visiting an EA Wiki entry.
Assumes that the entry was imported using the GraphQL query below
and converted to Markdown with Pandoc using `pandoc -s
[source.html] -t markdown -o [destination.md]'.

`{
  tag(input:{selector:{slug:\"[slug]\"}}) {
    result {
      name
      description {
	html
      }
      parentTag {
	name
      }
    }
  }
}'"
  (interactive)
  (when (not (eq major-mode 'markdown-mode))
    (user-error "Not in a Markdown buffer"))
  (save-excursion
    (unfill-region (point-min) (point-max))
    (goto-char (point-min))
    (while (re-search-forward "{.underline}" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "{.footnote-back-link}" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward ps/tlon-markdown-eawiki-footnote-source nil t)
      (replace-match (format "[^%s] " (match-string 1))))
    (goto-char (point-min))
    (while (re-search-forward ps/tlon-markdown-eawiki-footnote-source2 nil t)
      (replace-match (format "[^%s]: " (match-string 1))))
    (goto-char (point-min))
    (while (re-search-forward ps/tlon-markdown-eawiki-footnote-target nil t)
      (replace-match (format "[^%s]: " (match-string 1))))
    (goto-char (point-min))
    (while (re-search-forward ps/tlon-markdown-eawiki-footnote-target2 nil t)
      (replace-match (format "[^%s]: " (match-string 1))))
    (goto-char (point-min))
    (while (re-search-forward ps/tlon-markdown-eawiki-footnote-target3 nil t)
      (replace-match (format "[^%s]: " (match-string 1))))
    (goto-char (point-min))
    (while (re-search-forward ps/tlon-markdown-eawiki-footnote-target4 nil t)
      (replace-match (format "[^%s]: " (match-string 1))))
    (goto-char (point-min))
    (while (re-search-forward ps/tlon-markdown-eawiki-footnote-target5 nil t)
      (replace-match (format "[^%s]: " (match-string 1))))
    (goto-char (point-min))
    (while (re-search-forward ps/tlon-markdown-eawiki-links nil t)
      (replace-match (format "[%s](%s)" (match-string 1) (match-string 2)) nil t))
    (goto-char (point-min))
    (while (re-search-forward ps/tlon-markdown-eawiki-escaped-quotes nil t)
      (replace-match "\""))
    (goto-char (point-min))
    (while (re-search-forward " " nil t)
      (replace-match " "))
    (goto-char (point-min))
    (while (re-search-forward " :::" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "{.underline}" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "{.footnote-back-link}" nil t)
      (replace-match ""))
    (fill-region (point-min) (point-max))
    (save-buffer)
    ))

(defun ps/tlon-convert-to-markdown ()
  "Convert a file from EA Wiki to Markdown."
  (interactive)
  (dolist (file (directory-files "." nil "\\.md$"))
    ;; (shell-command (format "pandoc -s '%s' -t markdown -o '%s'"
    ;; file
    ;; (file-name-with-extension file "md")))
    (with-current-buffer (find-file-noselect (file-name-with-extension file "md"))
      (message "Cleaning up %s" (buffer-name))
      (ps/tlon-markdown-eawiki-cleanup))))

(defvar ps/tlon-bae-eawiki-slugs '("tag--impact"
				   "tag--1day-sooner"
				   "tag--80-000-hours"
				   "tag--80-000-hours-podcast"
				   "tag--ai-alignment-forum"
				   "tag--ai-impacts"
				   "tag--ai-safety-camp"
				   "tag--ai-safety-support"
				   "tag--ai-alignment"
				   "tag--ai-forecasting"
				   "tag--ai-governance"
				   "tag--ai-interpretability"
				   "tag--ai-race"
				   "tag--ai-risk"
				   "tag--ai-safety"
				   "tag--ai-safety-resources-and-materials"
				   "tag--ai-skepticism"
				   "tag--ai-takeoff"
				   "tag--allfed"
				   "tag--abdul-latif-jameel-poverty-action-lab"
				   "tag--academia-1"
				   "tag--accidental-harm"
				   "tag--adjusted-life-year"
				   "tag--against-malaria-foundation"
				   "tag--aging-research"
				   "tag--aid-and-paternalism"
				   "tag--air-pollution"
				   "tag--albert-schweitzer-foundation"
				   "tag--aligned-ai"
				   "tag--alignment-research-center"
				   "tag--alignment-tax"
				   "tag--all-party-parliamentary-group-for-future-generations"
				   "tag--altruistic-coordination"
				   "tag--altruistic-motivation"
				   "tag--altruistic-wager"
				   "tag--ambition"
				   "tag--anders-sandberg"
				   "tag--andreas-mogensen"
				   "tag--anima-international"
				   "tag--animal-advocacy-careers"
				   "tag--animal-ask"
				   "tag--animal-charity-evaluators"
				   "tag--animal-ethics"
				   "tag--animal-welfare-fund"
				   "tag--animal-product-alternatives"
				   "tag--animal-sentience"
				   "tag--animal-welfare"
				   "tag--anthropic"
				   "tag--anthropics"
				   "tag--anthropogenic-existential-risk"
				   "tag--antibiotic-resistance"
				   "tag--antitrust-law"
				   "tag--application-announcements"
				   "tag--applied-ethics"
				   "tag--april-fools-day-1"
				   "tag--arb"
				   "tag--architecture"
				   "tag--armed-conflict"
				   "tag--artificial-intelligence"
				   "tag--artificial-sentience"
				   "tag--asia"
				   "tag--ask-me-anything"
				   "tag--assistants"
				   "tag--asteroids"
				   "tag--astronomical-waste"
				   "tag--asymmetry"
				   "tag--atomically-precise-manufacturing"
				   "tag--audio"
				   "tag--automation"
				   "tag--autonomous-weapon"
				   "tag--axiology"
				   "tag--ballot-initiative"
				   "tag--bayes-theorem"
				   "tag--bayesian-epistemology"
				   "tag--berkeley-existential-risk-initiative"
				   "tag--biodiversity-loss"
				   "tag--bioethics"
				   "tag--biosecurity"
				   "tag--biosecurity-and-pandemics"
				   "tag--biosurveillance"
				   "tag--biotechnology"
				   "tag--bitcoin"
				   "tag--blockchain-technology"
				   "tag--blogging"
				   "tag--books"
				   "tag--bostrom-apology"
				   "tag--bounty-closed"
				   "tag--bounty-open"
				   "tag--brain-computer-interfaces"
				   "tag--brian-tomasik"
				   "tag--broad-vs-narrow-interventions"
				   "tag--bryan-caplan"
				   "tag--building-effective-altruism"
				   "tag--building-the-field-of-ai-safety"
				   "tag--burden-of-disease"
				   "tag--covid-19-pandemic"
				   "tag--california-effect"
				   "tag--cambridge-existential-risks-initiative"
				   "tag--career-advising"
				   "tag--career-capital"
				   "tag--career-choice"
				   "tag--career-framework"
				   "tag--carl-shulman"
				   "tag--carrick-flynn"
				   "tag--cash-transfers"
				   "tag--cause-exploration-prizes"
				   "tag--cause-innovation-bootcamp"
				   "tag--cause-x"
				   "tag--cause-candidates"
				   "tag--cause-neutrality"
				   "tag--cause-prioritization"
				   "tag--center-for-applied-rationality"
				   "tag--center-for-election-science"
				   "tag--center-for-human-compatible-artificial-intelligence"
				   "tag--center-for-reducing-suffering"
				   "tag--center-on-long-term-risk"
				   "tag--centre-for-effective-altruism-1"
				   "tag--centre-for-enabling-ea-learning-and-research"
				   "tag--centre-for-long-term-resilience"
				   "tag--centre-for-the-governance-of-ai"
				   "tag--centre-for-the-study-of-existential-risk"
				   "tag--certificate-of-impact"
				   "tag--charity-entrepreneurship"
				   "tag--charity-science-foundation"
				   "tag--charity-evaluation"
				   "tag--charity-incubation"
				   "tag--charter-cities"
				   "tag--chatgpt"
				   "tag--china"
				   "tag--civilizational-collapse"
				   "tag--classic-repost"
				   "tag--classical-utilitarianism"
				   "tag--climate-change"
				   "tag--climate-engineering"
				   "tag--cluelessness"
				   "tag--cluster-headache"
				   "tag--coaching"
				   "tag--cognitive-bias"
				   "tag--cognitive-decline"
				   "tag--cognitive-enhancement"
				   "tag--collections-and-resources"
				   "tag--college-advice"
				   "tag--communities-adjacent-to-effective-altruism"
				   "tag--community"
				   "tag--community-builder-writing-contest"
				   "tag--community-epistemic-health"
				   "tag--community-experiences"
				   "tag--community-housing"
				   "tag--community-infrastructure"
				   "tag--community-projects"
				   "tag--compassion-in-world-farming"
				   "tag--competitive-debating"
				   "tag--compute-governance"
				   "tag--conferences"
				   "tag--conflicts-of-interest"
				   "tag--conjecture"
				   "tag--conjunctive-vs-disjunctive-risk-models"
				   "tag--consciousness-research"
				   "tag--consequences-of-ai-timelines"
				   "tag--consequentialism"
				   "tag--conservation"
				   "tag--constraints-on-effective-altruism"
				   "tag--consultancy"
				   "tag--cooperative-ai-1"
				   "tag--corporate-animal-welfare-campaigns"
				   "tag--corporate-governance"
				   "tag--corruption"
				   "tag--cost-benefit-analysis"
				   "tag--cost-effectiveness"
				   "tag--cost-effectiveness-analysis"
				   "tag--counterfactual-reasoning"
				   "tag--coworking-spaces"
				   "tag--creative-writing-contest"
				   "tag--credal-resilience"
				   "tag--criminal-justice-reform"
				   "tag--criticism-and-red-teaming-contest"
				   "tag--criticism-of-effective-altruism"
				   "tag--criticism-of-effective-altruism-culture"
				   "tag--criticism-of-effective-altruist-causes"
				   "tag--criticism-of-effective-altruist-organizations"
				   "tag--criticism-of-longtermism-and-existential-risk-studies"
				   "tag--crucial-consideration"
				   "tag--crustacean-welfare"
				   "tag--crux"
				   "tag--cryonics"
				   "tag--cuban-missile-crisis"
				   "tag--cultivated-meat"
				   "tag--cultural-evolution"
				   "tag--cultural-persistence"
				   "tag--data-ea-community-1"
				   "tag--data-and-tech-projects"
				   "tag--data-science"
				   "tag--david-pearce-1"
				   "tag--decision-theory"
				   "tag--decision-theoretic-uncertainty"
				   "tag--deepmind"
				   "tag--defense-in-depth"
				   "tag--definition-of-effective-altruism"
				   "tag--demandingness-of-morality"
				   "tag--democracy"
				   "tag--deontology"
				   "tag--derek-parfit"
				   "tag--development-media-international"
				   "tag--deworming"
				   "tag--dietary-change"
				   "tag--differential-progress"
				   "tag--digital-person"
				   "tag--diminishing-returns"
				   "tag--discussion-norms"
				   "tag--disentanglement-research"
				   "tag--dispensers-for-safe-water"
				   "tag--distribution-of-cost-effectiveness"
				   "tag--diversity-and-inclusion"
				   "tag--divestment"
				   "tag--documentaries"
				   "tag--doing-good-better"
				   "tag--donation-choice"
				   "tag--donation-matching"
				   "tag--donation-pledge"
				   "tag--donation-writeup"
				   "tag--donor-lotteries"
				   "tag--doomsday-argument"
				   "tag--draft-amnesty-day"
				   "tag--dual-use"
				   "tag--dustin-moskovitz"
				   "tag--dylan-matthews"
				   "tag--dystopia"
				   "tag--ea-giving-tuesday"
				   "tag--ea-good-governance-project"
				   "tag--ea-librarian-project-inactive"
				   "tag--ea-organization-updates-monthly-series"
				   "tag--ea-talks-previously-earadio-project"
				   "tag--							  eagx"
				   "tag--earning-to-give"
				   "tag--eastern-europe"
				   "tag--economic-growth"
				   "tag--economic-inequality"
				   "tag--economics"
				   "tag--economics-of-artificial-intelligence"
				   "tag--education"
				   "tag--effective-altruism-anywhere"
				   "tag--effective-altruism-behavioral-science-newsletter"
				   "tag--effective-altruism-coaching"
				   "tag--effective-altruism-consulting-network"
				   "tag--effective-altruism-forum-1"
				   "tag--effective-altruism-foundation"
				   "tag--effective-altruism-funds"
				   "tag--effective-altruism-global"
				   "tag--effective-altruism-group-organisers-survey"
				   "tag--effective-altruism-hub"
				   "tag--effective-altruism-infrastructure-fund"
				   "tag--effective-altruism-israel"
				   "tag--effective-altruism-london"
				   "tag--effective-altruism-newsletter"
				   "tag--effective-altruism-policy-analytics"
				   "tag--effective-altruism-survey"
				   "tag--effective-altruism-for-christians"
				   "tag--effective-altruism-for-jews"
				   "tag--effective-institutions-project"
				   "tag--effective-thesis"
				   "tag--effective-ventures"
				   "tag--effective-altruism-art-and-fiction"
				   "tag--effective-altruism-culture"
				   "tag--effective-altruism-education"
				   "tag--effective-altruism-funding"
				   "tag--effective-altruism-groups"
				   "tag--effective-altruism-in-french"
				   "tag--effective-altruism-in-spanish"
				   "tag--effective-altruism-in-the-media"
				   "tag--effective-altruism-lifestyle"
				   "tag--effective-altruism-messaging"
				   "tag--effective-altruism-outreach-in-schools"
				   "tag--effective-animal-advocacy"
				   "tag--effective-giving"
				   "tag--effektiv-spenden-org"
				   "tag--electoral-politics"
				   "tag--electoral-reform"
				   "tag--eliezer-yudkowsky"
				   "tag--elon-musk"
				   "tag--emergency-response"
				   "tag--engineering"
				   "tag--entrepreneurship"
				   "tag--environmental-science"
				   "tag--epistemic-deference"
				   "tag--epistemology"
				   "tag--epoch"
				   "tag--estimate-elicitation-and-crowdsourcing"
				   "tag--estimation-of-existential-risk"
				   "tag--ethics-of-artificial-intelligence"
				   "tag--ethics-of-existential-risk"
				   "tag--ethics-of-personal-consumption"
				   "tag--european-union"
				   "tag--event-strategy"
				   "tag--evidence-action"
				   "tag--excited-vs-obligatory-altruism"
				   "tag--exercises"
				   "tag--existential-risk"
				   "tag--existential-risk-factor"
				   "tag--existential-risk-fiction"
				   "tag--existential-security"
				   "tag--expected-value"
				   "tag--expertise"
				   "tag--external-praise-for-effective-altruism"
				   "tag--extraterrestrial-intelligence"
				   "tag--ftx-foundation"
				   "tag--ftx-collapse"
				   "tag--ftx-collapse-related-resources"
				   "tag--fair-trade"
				   "tag--family-empowerment-media"
				   "tag--family-planning"
				   "tag--fanaticism"
				   "tag--farmed-animal-welfare"
				   "tag--faunalytics"
				   "tag--fellowships-and-internships"
				   "tag--fermi-estimate"
				   "tag--fermi-paradox"
				   "tag--field-building"
				   "tag--fiscal-sponsorship"
				   "tag--fish-welfare-initiative"
				   "tag--fish-welfare"
				   "tag--flourishing-futures"
				   "tag--food-security"
				   "tag--forecasting"
				   "tag--forecasting-and-estimation"
				   "tag--forecasting-newsletter"
				   "tag--foreign-aid"
				   "tag--foreign-aid-skepticism"
				   "tag--foresight-institute"
				   "tag--forethought-foundation"
				   "tag--fortify-health"
				   "tag--forum-prize"
				   "tag--forum-review"
				   "tag--founders-pledge"
				   "tag--funding-high-impact-for-profits"
				   "tag--funding-opportunities"
				   "tag--funding-request-open"
				   "tag--fundraising"
				   "tag--future-fund"
				   "tag--future-fund-worldview-prize"
				   "tag--future-matters"
				   "tag--future-perfect"
				   "tag--future-of-humanity-institute"
				   "tag--future-of-life-institute"
				   "tag--game-theory"
				   "tag--gene-drives"
				   "tag--generation-pledge"
				   "tag--geomagnetic-storms"
				   "tag--givedirectly"
				   "tag--givewell"
				   "tag--givewell-change-our-mind-contest"
				   "tag--giving-green"
				   "tag--giving-multiplier"
				   "tag--giving-pledge"
				   "tag--giving-what-we-can"
				   "tag--giving-what-we-can-newsletter"
				   "tag--giving-and-happiness-1"
				   "tag--global-outreach"
				   "tag--global-catastrophic-risk-institute"
				   "tag--global-health-and-development-fund"
				   "tag--global-priorities-institute"
				   "tag--global-catastrophic-biological-risk"
				   "tag--global-catastrophic-risk"
				   "tag--global-governance"
				   "tag--global-health-and-development"
				   "tag--global-health-and-wellbeing"
				   "tag--global-poverty"
				   "tag--global-priorities-research"
				   "tag--good-food-institute"
				   "tag--goodhart-s-law"
				   "tag--grabby-aliens"
				   "tag--grantmaking"
				   "tag--great-filter"
				   "tag--great-power-conflict"
				   "tag--greater-new-york-city-area"
				   "tag--guarding-against-pandemics"
				   "tag--guesstimate"
				   "tag--h5n1-virus"
				   "tag--hackathons"
				   "tag--happier-lives-institute"
				   "tag--hazards-vulnerabilities-and-exposure"
				   "tag--healthier-hens"
				   "tag--hear-this-idea"
				   "tag--hedonism"
				   "tag--high-impact-athletes-organization"
				   "tag--high-impact-medicine-group"
				   "tag--high-impact-professionals-organization"
				   "tag--hilary-greaves"
				   "tag--hinge-of-history"
				   "tag--hiring"
				   "tag--history"
				   "tag--history-of-effective-altruism"
				   "tag--history-of-existential-risk"
				   "tag--history-of-philanthropy"
				   "tag--holden-karnofsky"
				   "tag--human-compatible"
				   "tag--human-challenge-trials"
				   "tag--human-extinction"
				   "tag--humor-1"
				   "tag--idinsight"
				   "tag--itn-framework"
				   "tag--immigration-reform"
				   "tag--impact-assessment"
				   "tag--impact-investing-1"
				   "tag--impactful-government-careers"
				   "tag--impostor-syndrome"
				   "tag--improving-institutional-decision-making"
				   "tag--independent-impression"
				   "tag--independent-research"
				   "tag--india"
				   "tag--indirect-long-term-effects"
				   "tag--infinite-ethics"
				   "tag--information-hazard"
				   "tag--information-security"
				   "tag--insect-welfare"
				   "tag--inside-vs-outside-view"
				   "tag--instrumental-convergence-thesis"
				   "tag--intellectual-property"
				   "tag--intelligence-explosion"
				   "tag--international-organization"
				   "tag--international-relations"
				   "tag--international-trade"
				   "tag--interpersonal-comparisons-of-wellbeing"
				   "tag--intervention-evaluation"
				   "tag--intrinsic-vs-instrumental-value"
				   "tag--introduction-to-effective-altruism"
				   "tag--introductory-fellowship"
				   "tag--introspective-hedonism"
				   "tag--invertebrate-welfare"
				   "tag--investing"
				   "tag--iterated-embryo-selection"
				   "tag--jeremy-bentham"
				   "tag--job-listing-closed"
				   "tag--job-listing-open"
				   "tag--job-profile"
				   "tag--johns-hopkins-center-for-health-security"
				   "tag--journalism"
				   "tag--julia-galef"
				   "tag--kelsey-piper"
				   "tag--kidney-donation"
				   "tag--lean"
				   "tag--land-use-reform"
				   "tag--law"
				   "tag--lead-exposure-elimination-project"
				   "tag--leadership"
				   "tag--legal-priorities-project"
				   "tag--less-discussed-causes"
				   "tag--lesswrong"
				   "tag--lesswrong-for-ea"
				   "tag--leverage-research"
				   "tag--life-sciences"
				   "tag--list-of-tools-for-collaborative-truth-seeking"
				   "tag--local-priorities-research-1"
				   "tag--long-reflection"
				   "tag--long-term-future-fund"
				   "tag--long-range-forecasting"
				   "tag--long-term-future"
				   "tag--longtermism"
				   "tag--longtermist-entrepreneurship-fellowship"
				   "tag--longtermist-institutional-reform"
				   "tag--longview-philanthropy"
				   "tag--low-and-middle-income-countries"
				   "tag--machine-intelligence-research-institute"
				   "tag--macroeconomic-policy"
				   "tag--macrostrategy"
				   "tag--magnify-mentoring"
				   "tag--malaria"
				   "tag--malaria-consortium"
				   "tag--management-and-mentoring"
				   "tag--manifold-markets"
				   "tag--marketing"
				   "tag--markets-for-altruism"
				   "tag--measuring-and-comparing-value"
				   "tag--meat-eater-problem"
				   "tag--mechanism-design"
				   "tag--medicine"
				   "tag--meditation"
				   "tag--megaprojects"
				   "tag--meme"
				   "tag--mental-health-navigator"
				   "tag--mental-health"
				   "tag--mercy-for-animals"
				   "tag--metaculus"
				   "tag--metaethics"
				   "tag--metascience"
				   "tag--michael-huemer"
				   "tag--micronutrient-deficiency"
				   "tag--misinformation-1"
				   "tag--mission-correlated-investing"
				   "tag--model-uncertainty"
				   "tag--models"
				   "tag--monthly-overload-of-effective-altruism"
				   "tag--moral-advocacy"
				   "tag--moral-circle-expansion-1"
				   "tag--moral-cooperation"
				   "tag--moral-offsetting"
				   "tag--moral-patienthood"
				   "tag--moral-philosophy"
				   "tag--moral-psychology"
				   "tag--moral-trade"
				   "tag--moral-uncertainty"
				   "tag--moral-weight"
				   "tag--motivational"
				   "tag--movement-collapse"
				   "tag--naive-vs-sophisticated-consequentialism"
				   "tag--natural-existential-risk"
				   "tag--near-term-ai-ethics"
				   "tag--negative-utilitarianism"
				   "tag--neglectedness"
				   "tag--network-building"
				   "tag--neurotechnology"
				   "tag--neutrality"
				   "tag--new-harvest"
				   "tag--news-relevant-to-effective-altruism"
				   "tag--newsletters"
				   "tag--nick-bostrom"
				   "tag--non-humans-and-the-long-term-future"
				   "tag--non-wellbeing-sources-of-value"
				   "tag--nonlinear-fund"
				   "tag--nonprofit-governance"
				   "tag--normative-ethics"
				   "tag--north-korea"
				   "tag--nuclear-energy"
				   "tag--nuclear-security"
				   "tag--nuclear-warfare-1"
				   "tag--nuclear-winter"
				   "tag--obituary"
				   "tag--one-for-the-world-1"
				   "tag--online-effective-altruism-communities"
				   "tag--open-philanthropy"
				   "tag--open-thread"
				   "tag--openai"
				   "tag--operations"
				   "tag--operations-research"
				   "tag--optimizer-s-curse"
				   "tag--organisation-for-the-prevention-of-intense-suffering"
				   "tag--organization-strategy"
				   "tag--organization-updates"
				   "tag--orthogonality-thesis"
				   "tag--other-moral-theories"
				   "tag--ought"
				   "tag--our-world-in-data"
				   "tag--pain-and-suffering"
				   "tag--pandemic-preparedness"
				   "tag--parenting"
				   "tag--pascal-s-mugging"
				   "tag--patient-altruism"
				   "tag--paul-christiano"
				   "tag--peace-and-conflict-studies"
				   "tag--person-affecting-views"
				   "tag--personal-development"
				   "tag--personal-finance"
				   "tag--personal-fit"
				   "tag--personal-identity"
				   "tag--peter-singer"
				   "tag--petrov-day"
				   "tag--philanthropic-coordination"
				   "tag--philanthropic-diversification"
				   "tag--philip-tetlock"
				   "tag--philippines"
				   "tag--philosophy"
				   "tag--philosophy-of-effective-altruism"
				   "tag--philosophy-of-mind"
				   "tag--pineapple-operations"
				   "tag--podcasts"
				   "tag--policy"
				   "tag--political-polarization"
				   "tag--population-decline"
				   "tag--population-ethics"
				   "tag--postmortems-and-retrospectives"
				   "tag--power-dynamics-in-effective-altruism"
				   "tag--practical"
				   "tag--prediction-markets"
				   "tag--prize"
				   "tag--probably-good"
				   "tag--product-management"
				   "tag--productivity"
				   "tag--progress-studies"
				   "tag--project-for-awesome"
				   "tag--project-voting"
				   "tag--proliferation"
				   "tag--psychedelics"
				   "tag--psychology"
				   "tag--psychotherapy"
				   "tag--public-giving"
				   "tag--public-goods"
				   "tag--public-interest-technology"
				   "tag--public-relations"
				   "tag--publication-norms"
				   "tag--qualia-research-institute"
				   "tag--quantified-uncertainty-research-institute"
				   "tag--quantum-computing"
				   "tag--quotes"
				   "tag--rc-forward"
				   "tag--raising-for-effective-giving"
				   "tag--randomized-controlled-trials"
				   "tag--rationality"
				   "tag--rationality-community"
				   "tag--ready-research"
				   "tag--reasoning-transparency"
				   "tag--red-teaming"
				   "tag--redwood-research"
				   "tag--refuges"
				   "tag--religion"
				   "tag--replaceability"
				   "tag--repugnant-conclusion"
				   "tag--request-for-proposal"
				   "tag--requests-closed"
				   "tag--requests-open"
				   "tag--research"
				   "tag--research-agendas-questions-and-project-lists"
				   "tag--research-careers"
				   "tag--research-methods"
				   "tag--research-summary"
				   "tag--research-training-programs"
				   "tag--resilient-food"
				   "tag--resource-tradeoffs"
				   "tag--rethink-charity"
				   "tag--rethink-priorities"
				   "tag--review-crosspost"
				   "tag--risk-assessment"
				   "tag--risk-aversion"
				   "tag--risks-from-malevolent-actors"
				   "tag--robin-hanson"
				   "tag--role-impact"
				   "tag--russia"
				   "tag--s-risk"
				   "tag--sci-foundation"
				   "tag--spc-framework"
				   "tag--safeguarding-liberal-democracy"
				   "tag--sam-bankman-fried"
				   "tag--samotsvety-forecasting"
				   "tag--scalably-using-labour"
				   "tag--science-and-technology-studies"
				   "tag--scientific-progress"
				   "tag--scope-neglect"
				   "tag--scott-alexander"
				   "tag--scout-mindset"
				   "tag--self-care"
				   "tag--semiconductors"
				   "tag--sentience-1"
				   "tag--sentience-institute"
				   "tag--shrimp-welfare-project"
				   "tag--simulation-argument"
				   "tag--smallpox-eradication-programme"
				   "tag--sogive"
				   "tag--social-and-intellectual-movements"
				   "tag--social-science-1"
				   "tag--socially-responsible-investing"
				   "tag--software-engineering"
				   "tag--space-colonization"
				   "tag--space-governance"
				   "tag--spaced-repetition"
				   "tag--spark-wave"
				   "tag--speciesism"
				   "tag--speeding-up-development"
				   "tag--spillover-effects"
				   "tag--squiggle"
				   "tag--standards-and-regulation"
				   "tag--stanford-existential-risks-initiative"
				   "tag--stanislav-petrov"
				   "tag--statistics"
				   "tag--status-quo-bias"
				   "tag--steven-pinker"
				   "tag--strongminds"
				   "tag--student-projects"
				   "tag--students-for-high-impact-charity"
				   "tag--subjective-wellbeing"
				   "tag--suffering-focused-ethics"
				   "tag--summer-program-on-applied-rationality-and-cognition"
				   "tag--superintelligence"
				   "tag--superintelligence-book"
				   "tag--supervolcano"
				   "tag--surveillance"
				   "tag--surveys"
				   "tag--survival-and-flourishing"
				   "tag--suvita"
				   "tag--swiss-existential-risk-initiative"
				   "tag--systemic-change"
				   "tag--tabletop-exercises"
				   "tag--take-action"
				   "tag--task-y"
				   "tag--teaching-materials"
				   "tag--technology-race"
				   "tag--temporal-discounting"
				   "tag--terrorism"
				   "tag--the-humane-league"
				   "tag--the-life-you-can-save"
				   "tag--the-precipice"
				   "tag--the-unjournal"
				   "tag--theory-of-change"
				   "tag--think-tanks"
				   "tag--thinking-at-the-margin"
				   "tag--time-of-perils"
				   "tag--timing-of-philanthropy"
				   "tag--tobacco-control"
				   "tag--toby-ord"
				   "tag--total-view"
				   "tag--totalitarianism"
				   "tag--tractability"
				   "tag--training-for-good"
				   "tag--trajectory-change"
				   "tag--transformative-artificial-intelligence"
				   "tag--transhumanism"
				   "tag--translation"
				   "tag--transparency"
				   "tag--twitter"
				   "tag--tyler-cowen"
				   "tag--uk-policy"
				   "tag--us-policy"
				   "tag--ukraine"
				   "tag--united-kingdom"
				   "tag--united-nations-1"
				   "tag--united-states"
				   "tag--universal-basic-income"
				   "tag--university-groups"
				   "tag--utilitarianism"
				   "tag--vaccines"
				   "tag--valence"
				   "tag--value-drift"
				   "tag--value-lock-in"
				   "tag--value-of-information"
				   "tag--value-of-movement-growth"
				   "tag--video-1"
				   "tag--virtue-ethics"
				   "tag--vitalik-buterin"
				   "tag--volcanic-winter"
				   "tag--volunteering"
				   "tag--vulnerable-world-hypothesis"
				   "tag--warning-shot"
				   "tag--wave"
				   "tag--weapons-of-mass-destruction"
				   "tag--weekly-summaries-project"
				   "tag--welfare-biology"
				   "tag--welfare-economics"
				   "tag--welfarism"
				   "tag--wellbeing"
				   "tag--what-we-owe-the-future"
				   "tag--whole-brain-emulation"
				   "tag--wikis"
				   "tag--wild-animal-initiative"
				   "tag--wild-animal-welfare"
				   "tag--william-macaskill"
				   "tag--windfall-clause"
				   "tag--women-s-health-and-welfare"
				   "tag--working-at-ea-vs-non-ea-orgs"
				   "tag--workplace-advocacy"
				   "tag--workplace-groups"
				   "tag--workshops-retreats-summits"
				   "tag--worldview-diversity-in-effective-altruism"
				   "tag--writing-advice"))

(defun ps/tlon-bae-insert-eawiki-slug ()
  "Insert an EA Wiki slug at point."
  (interactive)
  (insert (completing-read "URL: " ps/tlon-bae-eawiki-slugs)))

(defun ps/tlon-bae-initialize-revision ()
  "Create new branch named after current buffer."
  (interactive)
  (let ((new-branch (buffer-name (current-buffer))))
    (unless (magit-branch-p new-branch)
      (magit-branch-create new-branch "main"))
    (magit-branch-checkout new-branch)
    (message "Now at branch %s" new-branch)))

(defun ps/tlon-bae-finalize-translation ()
  "Finalize BAE translation.
With point in a buffer that contains a finished BAE translation,
 perform the following operations:

1. `fill-region' on the entire buffer.
2. `save-buffer'.
3. stage changes in local repo.
4. prepopulate commit with relevant message."
  (fill-region (point-min) (point-max))
  (save-buffer)
  (let* ((commit-summary-minus-filename commit-type)
	 (commit-summary (concat
			  commit-summary-minus-filename
			  (truncate-string-to-width
			   (buffer-name)
			   (- git-commit-summary-max-length
			      (length commit-summary-minus-filename))))))
    (setq ps/git-commit-setup-message commit-summary)
    (magit-commit-create)))

(defun ps/tlon-bae-finalize-revision ()
  "Finalize BAE revision.
With point in a buffer that contains a finished BAE translation,
 perform the following operations:

1. `fill-region' on the entire buffer.
2. `save-buffer'.
3. stage changes in local repo.
4. commit with relevant message.
5. push to remote.
6. create pull request."
  (interactive)
  (fill-region (point-min) (point-max))
  (save-buffer)
  (let* ((commit-summary-minus-filename "Revise ")
	 (commit-summary (concat
			  commit-summary-minus-filename
			  (truncate-string-to-width
			   (buffer-name)
			   (- git-commit-summary-max-length
			      (length commit-summary-minus-filename))))))
    (magit-commit-create (list "-m" commit-summary))
    (call-interactively #'magit-push-current-to-pushremote)
    (call-interactively '(lambda (forge-create-pullreq (magit-get-current-branch) "origin/main")))))

(defvar ps/git-commit-setup-message
  ""
  "Message to pre-populate a commit.

This message is inserted by `ps/git-commit-setup', which is in
turn triggered by `git-commit-setup-hook'.")

(defun ps/git-commit-setup ()
  "Setup a commit message."
  (insert ps/git-commit-setup-message)
  (setq ps/git-commit-setup-message ""))

(add-hook 'git-commit-setup-hook 'ps/git-commit-setup)

(defun ps/tlon-elpaca-update-and-rebuild ()
  "Update and rebuild Tlon package with Elpaca."
  (interactive)
  (elpaca-update 'tlon t)
  (elpaca-rebuild 'tlon t))

(provide 'tlon)
;;; tlon.el ends here
