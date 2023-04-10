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

(defvar ps/tlon-bae-eawiki-slugs '("impact"
				   "1day-sooner"
				   "80-000-hours"
				   "80-000-hours-podcast"
				   "ai-alignment-forum"
				   "ai-impacts"
				   "ai-safety-camp"
				   "ai-safety-support"
				   "ai-alignment"
				   "ai-forecasting"
				   "ai-governance"
				   "ai-interpretability"
				   "ai-race"
				   "ai-risk"
				   "ai-safety"
				   "ai-safety-resources-and-materials"
				   "ai-skepticism"
				   "ai-takeoff"
				   "allfed"
				   "abdul-latif-jameel-poverty-action-lab"
				   "academia-1"
				   "accidental-harm"
				   "adjusted-life-year"
				   "against-malaria-foundation"
				   "aging-research"
				   "aid-and-paternalism"
				   "air-pollution"
				   "albert-schweitzer-foundation"
				   "aligned-ai"
				   "alignment-research-center"
				   "alignment-tax"
				   "all-party-parliamentary-group-for-future-generations"
				   "altruistic-coordination"
				   "altruistic-motivation"
				   "altruistic-wager"
				   "ambition"
				   "anders-sandberg"
				   "andreas-mogensen"
				   "anima-international"
				   "animal-advocacy-careers"
				   "animal-ask"
				   "animal-charity-evaluators"
				   "animal-ethics"
				   "animal-welfare-fund"
				   "animal-product-alternatives"
				   "animal-sentience"
				   "animal-welfare"
				   "anthropic"
				   "anthropics"
				   "anthropogenic-existential-risk"
				   "antibiotic-resistance"
				   "antitrust-law"
				   "application-announcements"
				   "applied-ethics"
				   "april-fools-day-1"
				   "arb"
				   "architecture"
				   "armed-conflict"
				   "artificial-intelligence"
				   "artificial-sentience"
				   "asia"
				   "ask-me-anything"
				   "assistants"
				   "asteroids"
				   "astronomical-waste"
				   "asymmetry"
				   "atomically-precise-manufacturing"
				   "audio"
				   "automation"
				   "autonomous-weapon"
				   "axiology"
				   "ballot-initiative"
				   "bayes-theorem"
				   "bayesian-epistemology"
				   "berkeley-existential-risk-initiative"
				   "biodiversity-loss"
				   "bioethics"
				   "biosecurity"
				   "biosecurity-and-pandemics"
				   "biosurveillance"
				   "biotechnology"
				   "bitcoin"
				   "blockchain-technology"
				   "blogging"
				   "books"
				   "bostrom-apology"
				   "bounty-closed"
				   "bounty-open"
				   "brain-computer-interfaces"
				   "brian-tomasik"
				   "broad-vs-narrow-interventions"
				   "bryan-caplan"
				   "building-effective-altruism"
				   "building-the-field-of-ai-safety"
				   "burden-of-disease"
				   "covid-19-pandemic"
				   "california-effect"
				   "cambridge-existential-risks-initiative"
				   "career-advising"
				   "career-capital"
				   "career-choice"
				   "career-framework"
				   "carl-shulman"
				   "carrick-flynn"
				   "cash-transfers"
				   "cause-exploration-prizes"
				   "cause-innovation-bootcamp"
				   "cause-x"
				   "cause-candidates"
				   "cause-neutrality"
				   "cause-prioritization"
				   "center-for-applied-rationality"
				   "center-for-election-science"
				   "center-for-human-compatible-artificial-intelligence"
				   "center-for-reducing-suffering"
				   "center-on-long-term-risk"
				   "centre-for-effective-altruism-1"
				   "centre-for-enabling-ea-learning-and-research"
				   "centre-for-long-term-resilience"
				   "centre-for-the-governance-of-ai"
				   "centre-for-the-study-of-existential-risk"
				   "certificate-of-impact"
				   "charity-entrepreneurship"
				   "charity-science-foundation"
				   "charity-evaluation"
				   "charity-incubation"
				   "charter-cities"
				   "chatgpt"
				   "china"
				   "civilizational-collapse"
				   "classic-repost"
				   "classical-utilitarianism"
				   "climate-change"
				   "climate-engineering"
				   "cluelessness"
				   "cluster-headache"
				   "coaching"
				   "cognitive-bias"
				   "cognitive-decline"
				   "cognitive-enhancement"
				   "collections-and-resources"
				   "college-advice"
				   "communities-adjacent-to-effective-altruism"
				   "community"
				   "community-builder-writing-contest"
				   "community-epistemic-health"
				   "community-experiences"
				   "community-housing"
				   "community-infrastructure"
				   "community-projects"
				   "compassion-in-world-farming"
				   "competitive-debating"
				   "compute-governance"
				   "conferences"
				   "conflicts-of-interest"
				   "conjecture"
				   "conjunctive-vs-disjunctive-risk-models"
				   "consciousness-research"
				   "consequences-of-ai-timelines"
				   "consequentialism"
				   "conservation"
				   "constraints-on-effective-altruism"
				   "consultancy"
				   "cooperative-ai-1"
				   "corporate-animal-welfare-campaigns"
				   "corporate-governance"
				   "corruption"
				   "cost-benefit-analysis"
				   "cost-effectiveness"
				   "cost-effectiveness-analysis"
				   "counterfactual-reasoning"
				   "coworking-spaces"
				   "creative-writing-contest"
				   "credal-resilience"
				   "criminal-justice-reform"
				   "criticism-and-red-teaming-contest"
				   "criticism-of-effective-altruism"
				   "criticism-of-effective-altruism-culture"
				   "criticism-of-effective-altruist-causes"
				   "criticism-of-effective-altruist-organizations"
				   "criticism-of-longtermism-and-existential-risk-studies"
				   "crucial-consideration"
				   "crustacean-welfare"
				   "crux"
				   "cryonics"
				   "cuban-missile-crisis"
				   "cultivated-meat"
				   "cultural-evolution"
				   "cultural-persistence"
				   "data-ea-community-1"
				   "data-and-tech-projects"
				   "data-science"
				   "david-pearce-1"
				   "decision-theory"
				   "decision-theoretic-uncertainty"
				   "deepmind"
				   "defense-in-depth"
				   "definition-of-effective-altruism"
				   "demandingness-of-morality"
				   "democracy"
				   "deontology"
				   "derek-parfit"
				   "development-media-international"
				   "deworming"
				   "dietary-change"
				   "differential-progress"
				   "digital-person"
				   "diminishing-returns"
				   "discussion-norms"
				   "disentanglement-research"
				   "dispensers-for-safe-water"
				   "distribution-of-cost-effectiveness"
				   "diversity-and-inclusion"
				   "divestment"
				   "documentaries"
				   "doing-good-better"
				   "donation-choice"
				   "donation-matching"
				   "donation-pledge"
				   "donation-writeup"
				   "donor-lotteries"
				   "doomsday-argument"
				   "draft-amnesty-day"
				   "dual-use"
				   "dustin-moskovitz"
				   "dylan-matthews"
				   "dystopia"
				   "ea-giving-tuesday"
				   "ea-good-governance-project"
				   "ea-librarian-project-inactive"
				   "ea-organization-updates-monthly-series"
				   "ea-talks-previously-earadio-project"
				   "							  eagx"
				   "earning-to-give"
				   "eastern-europe"
				   "economic-growth"
				   "economic-inequality"
				   "economics"
				   "economics-of-artificial-intelligence"
				   "education"
				   "effective-altruism-anywhere"
				   "effective-altruism-behavioral-science-newsletter"
				   "effective-altruism-coaching"
				   "effective-altruism-consulting-network"
				   "effective-altruism-forum-1"
				   "effective-altruism-foundation"
				   "effective-altruism-funds"
				   "effective-altruism-global"
				   "effective-altruism-group-organisers-survey"
				   "effective-altruism-hub"
				   "effective-altruism-infrastructure-fund"
				   "effective-altruism-israel"
				   "effective-altruism-london"
				   "effective-altruism-newsletter"
				   "effective-altruism-policy-analytics"
				   "effective-altruism-survey"
				   "effective-altruism-for-christians"
				   "effective-altruism-for-jews"
				   "effective-institutions-project"
				   "effective-thesis"
				   "effective-ventures"
				   "effective-altruism-art-and-fiction"
				   "effective-altruism-culture"
				   "effective-altruism-education"
				   "effective-altruism-funding"
				   "effective-altruism-groups"
				   "effective-altruism-in-french"
				   "effective-altruism-in-spanish"
				   "effective-altruism-in-the-media"
				   "effective-altruism-lifestyle"
				   "effective-altruism-messaging"
				   "effective-altruism-outreach-in-schools"
				   "effective-animal-advocacy"
				   "effective-giving"
				   "effektiv-spenden-org"
				   "electoral-politics"
				   "electoral-reform"
				   "eliezer-yudkowsky"
				   "elon-musk"
				   "emergency-response"
				   "engineering"
				   "entrepreneurship"
				   "environmental-science"
				   "epistemic-deference"
				   "epistemology"
				   "epoch"
				   "estimate-elicitation-and-crowdsourcing"
				   "estimation-of-existential-risk"
				   "ethics-of-artificial-intelligence"
				   "ethics-of-existential-risk"
				   "ethics-of-personal-consumption"
				   "european-union"
				   "event-strategy"
				   "evidence-action"
				   "excited-vs-obligatory-altruism"
				   "exercises"
				   "existential-risk"
				   "existential-risk-factor"
				   "existential-risk-fiction"
				   "existential-security"
				   "expected-value"
				   "expertise"
				   "external-praise-for-effective-altruism"
				   "extraterrestrial-intelligence"
				   "ftx-foundation"
				   "ftx-collapse"
				   "ftx-collapse-related-resources"
				   "fair-trade"
				   "family-empowerment-media"
				   "family-planning"
				   "fanaticism"
				   "farmed-animal-welfare"
				   "faunalytics"
				   "fellowships-and-internships"
				   "fermi-estimate"
				   "fermi-paradox"
				   "field-building"
				   "fiscal-sponsorship"
				   "fish-welfare-initiative"
				   "fish-welfare"
				   "flourishing-futures"
				   "food-security"
				   "forecasting"
				   "forecasting-and-estimation"
				   "forecasting-newsletter"
				   "foreign-aid"
				   "foreign-aid-skepticism"
				   "foresight-institute"
				   "forethought-foundation"
				   "fortify-health"
				   "forum-prize"
				   "forum-review"
				   "founders-pledge"
				   "funding-high-impact-for-profits"
				   "funding-opportunities"
				   "funding-request-open"
				   "fundraising"
				   "future-fund"
				   "future-fund-worldview-prize"
				   "future-matters"
				   "future-perfect"
				   "future-of-humanity-institute"
				   "future-of-life-institute"
				   "game-theory"
				   "gene-drives"
				   "generation-pledge"
				   "geomagnetic-storms"
				   "givedirectly"
				   "givewell"
				   "givewell-change-our-mind-contest"
				   "giving-green"
				   "giving-multiplier"
				   "giving-pledge"
				   "giving-what-we-can"
				   "giving-what-we-can-newsletter"
				   "giving-and-happiness-1"
				   "global-outreach"
				   "global-catastrophic-risk-institute"
				   "global-health-and-development-fund"
				   "global-priorities-institute"
				   "global-catastrophic-biological-risk"
				   "global-catastrophic-risk"
				   "global-governance"
				   "global-health-and-development"
				   "global-health-and-wellbeing"
				   "global-poverty"
				   "global-priorities-research"
				   "good-food-institute"
				   "goodhart-s-law"
				   "grabby-aliens"
				   "grantmaking"
				   "great-filter"
				   "great-power-conflict"
				   "greater-new-york-city-area"
				   "guarding-against-pandemics"
				   "guesstimate"
				   "h5n1-virus"
				   "hackathons"
				   "happier-lives-institute"
				   "hazards-vulnerabilities-and-exposure"
				   "healthier-hens"
				   "hear-this-idea"
				   "hedonism"
				   "high-impact-athletes-organization"
				   "high-impact-medicine-group"
				   "high-impact-professionals-organization"
				   "hilary-greaves"
				   "hinge-of-history"
				   "hiring"
				   "history"
				   "history-of-effective-altruism"
				   "history-of-existential-risk"
				   "history-of-philanthropy"
				   "holden-karnofsky"
				   "human-compatible"
				   "human-challenge-trials"
				   "human-extinction"
				   "humor-1"
				   "idinsight"
				   "itn-framework"
				   "immigration-reform"
				   "impact-assessment"
				   "impact-investing-1"
				   "impactful-government-careers"
				   "impostor-syndrome"
				   "improving-institutional-decision-making"
				   "independent-impression"
				   "independent-research"
				   "india"
				   "indirect-long-term-effects"
				   "infinite-ethics"
				   "information-hazard"
				   "information-security"
				   "insect-welfare"
				   "inside-vs-outside-view"
				   "instrumental-convergence-thesis"
				   "intellectual-property"
				   "intelligence-explosion"
				   "international-organization"
				   "international-relations"
				   "international-trade"
				   "interpersonal-comparisons-of-wellbeing"
				   "intervention-evaluation"
				   "intrinsic-vs-instrumental-value"
				   "introduction-to-effective-altruism"
				   "introductory-fellowship"
				   "introspective-hedonism"
				   "invertebrate-welfare"
				   "investing"
				   "iterated-embryo-selection"
				   "jeremy-bentham"
				   "job-listing-closed"
				   "job-listing-open"
				   "job-profile"
				   "johns-hopkins-center-for-health-security"
				   "journalism"
				   "julia-galef"
				   "kelsey-piper"
				   "kidney-donation"
				   "lean"
				   "land-use-reform"
				   "law"
				   "lead-exposure-elimination-project"
				   "leadership"
				   "legal-priorities-project"
				   "less-discussed-causes"
				   "lesswrong"
				   "lesswrong-for-ea"
				   "leverage-research"
				   "life-sciences"
				   "list-of-tools-for-collaborative-truth-seeking"
				   "local-priorities-research-1"
				   "long-reflection"
				   "long-term-future-fund"
				   "long-range-forecasting"
				   "long-term-future"
				   "longtermism"
				   "longtermist-entrepreneurship-fellowship"
				   "longtermist-institutional-reform"
				   "longview-philanthropy"
				   "low-and-middle-income-countries"
				   "machine-intelligence-research-institute"
				   "macroeconomic-policy"
				   "macrostrategy"
				   "magnify-mentoring"
				   "malaria"
				   "malaria-consortium"
				   "management-and-mentoring"
				   "manifold-markets"
				   "marketing"
				   "markets-for-altruism"
				   "measuring-and-comparing-value"
				   "meat-eater-problem"
				   "mechanism-design"
				   "medicine"
				   "meditation"
				   "megaprojects"
				   "meme"
				   "mental-health-navigator"
				   "mental-health"
				   "mercy-for-animals"
				   "metaculus"
				   "metaethics"
				   "metascience"
				   "michael-huemer"
				   "micronutrient-deficiency"
				   "misinformation-1"
				   "mission-correlated-investing"
				   "model-uncertainty"
				   "models"
				   "monthly-overload-of-effective-altruism"
				   "moral-advocacy"
				   "moral-circle-expansion-1"
				   "moral-cooperation"
				   "moral-offsetting"
				   "moral-patienthood"
				   "moral-philosophy"
				   "moral-psychology"
				   "moral-trade"
				   "moral-uncertainty"
				   "moral-weight"
				   "motivational"
				   "movement-collapse"
				   "naive-vs-sophisticated-consequentialism"
				   "natural-existential-risk"
				   "near-term-ai-ethics"
				   "negative-utilitarianism"
				   "neglectedness"
				   "network-building"
				   "neurotechnology"
				   "neutrality"
				   "new-harvest"
				   "news-relevant-to-effective-altruism"
				   "newsletters"
				   "nick-bostrom"
				   "non-humans-and-the-long-term-future"
				   "non-wellbeing-sources-of-value"
				   "nonlinear-fund"
				   "nonprofit-governance"
				   "normative-ethics"
				   "north-korea"
				   "nuclear-energy"
				   "nuclear-security"
				   "nuclear-warfare-1"
				   "nuclear-winter"
				   "obituary"
				   "one-for-the-world-1"
				   "online-effective-altruism-communities"
				   "open-philanthropy"
				   "open-thread"
				   "openai"
				   "operations"
				   "operations-research"
				   "optimizer-s-curse"
				   "organisation-for-the-prevention-of-intense-suffering"
				   "organization-strategy"
				   "organization-updates"
				   "orthogonality-thesis"
				   "other-moral-theories"
				   "ought"
				   "our-world-in-data"
				   "pain-and-suffering"
				   "pandemic-preparedness"
				   "parenting"
				   "pascal-s-mugging"
				   "patient-altruism"
				   "paul-christiano"
				   "peace-and-conflict-studies"
				   "person-affecting-views"
				   "personal-development"
				   "personal-finance"
				   "personal-fit"
				   "personal-identity"
				   "peter-singer"
				   "petrov-day"
				   "philanthropic-coordination"
				   "philanthropic-diversification"
				   "philip-tetlock"
				   "philippines"
				   "philosophy"
				   "philosophy-of-effective-altruism"
				   "philosophy-of-mind"
				   "pineapple-operations"
				   "podcasts"
				   "policy"
				   "political-polarization"
				   "population-decline"
				   "population-ethics"
				   "postmortems-and-retrospectives"
				   "power-dynamics-in-effective-altruism"
				   "practical"
				   "prediction-markets"
				   "prize"
				   "probably-good"
				   "product-management"
				   "productivity"
				   "progress-studies"
				   "project-for-awesome"
				   "project-voting"
				   "proliferation"
				   "psychedelics"
				   "psychology"
				   "psychotherapy"
				   "public-giving"
				   "public-goods"
				   "public-interest-technology"
				   "public-relations"
				   "publication-norms"
				   "qualia-research-institute"
				   "quantified-uncertainty-research-institute"
				   "quantum-computing"
				   "quotes"
				   "rc-forward"
				   "raising-for-effective-giving"
				   "randomized-controlled-trials"
				   "rationality"
				   "rationality-community"
				   "ready-research"
				   "reasoning-transparency"
				   "red-teaming"
				   "redwood-research"
				   "refuges"
				   "religion"
				   "replaceability"
				   "repugnant-conclusion"
				   "request-for-proposal"
				   "requests-closed"
				   "requests-open"
				   "research"
				   "research-agendas-questions-and-project-lists"
				   "research-careers"
				   "research-methods"
				   "research-summary"
				   "research-training-programs"
				   "resilient-food"
				   "resource-tradeoffs"
				   "rethink-charity"
				   "rethink-priorities"
				   "review-crosspost"
				   "risk-assessment"
				   "risk-aversion"
				   "risks-from-malevolent-actors"
				   "robin-hanson"
				   "role-impact"
				   "russia"
				   "s-risk"
				   "sci-foundation"
				   "spc-framework"
				   "safeguarding-liberal-democracy"
				   "sam-bankman-fried"
				   "samotsvety-forecasting"
				   "scalably-using-labour"
				   "science-and-technology-studies"
				   "scientific-progress"
				   "scope-neglect"
				   "scott-alexander"
				   "scout-mindset"
				   "self-care"
				   "semiconductors"
				   "sentience-1"
				   "sentience-institute"
				   "shrimp-welfare-project"
				   "simulation-argument"
				   "smallpox-eradication-programme"
				   "sogive"
				   "social-and-intellectual-movements"
				   "social-science-1"
				   "socially-responsible-investing"
				   "software-engineering"
				   "space-colonization"
				   "space-governance"
				   "spaced-repetition"
				   "spark-wave"
				   "speciesism"
				   "speeding-up-development"
				   "spillover-effects"
				   "squiggle"
				   "standards-and-regulation"
				   "stanford-existential-risks-initiative"
				   "stanislav-petrov"
				   "statistics"
				   "status-quo-bias"
				   "steven-pinker"
				   "strongminds"
				   "student-projects"
				   "students-for-high-impact-charity"
				   "subjective-wellbeing"
				   "suffering-focused-ethics"
				   "summer-program-on-applied-rationality-and-cognition"
				   "superintelligence"
				   "superintelligence-book"
				   "supervolcano"
				   "surveillance"
				   "surveys"
				   "survival-and-flourishing"
				   "suvita"
				   "swiss-existential-risk-initiative"
				   "systemic-change"
				   "tabletop-exercises"
				   "take-action"
				   "task-y"
				   "teaching-materials"
				   "technology-race"
				   "temporal-discounting"
				   "terrorism"
				   "the-humane-league"
				   "the-life-you-can-save"
				   "the-precipice"
				   "the-unjournal"
				   "theory-of-change"
				   "think-tanks"
				   "thinking-at-the-margin"
				   "time-of-perils"
				   "timing-of-philanthropy"
				   "tobacco-control"
				   "toby-ord"
				   "total-view"
				   "totalitarianism"
				   "tractability"
				   "training-for-good"
				   "trajectory-change"
				   "transformative-artificial-intelligence"
				   "transhumanism"
				   "translation"
				   "transparency"
				   "twitter"
				   "tyler-cowen"
				   "uk-policy"
				   "us-policy"
				   "ukraine"
				   "united-kingdom"
				   "united-nations-1"
				   "united-states"
				   "universal-basic-income"
				   "university-groups"
				   "utilitarianism"
				   "vaccines"
				   "valence"
				   "value-drift"
				   "value-lock-in"
				   "value-of-information"
				   "value-of-movement-growth"
				   "video-1"
				   "virtue-ethics"
				   "vitalik-buterin"
				   "volcanic-winter"
				   "volunteering"
				   "vulnerable-world-hypothesis"
				   "warning-shot"
				   "wave"
				   "weapons-of-mass-destruction"
				   "weekly-summaries-project"
				   "welfare-biology"
				   "welfare-economics"
				   "welfarism"
				   "wellbeing"
				   "what-we-owe-the-future"
				   "whole-brain-emulation"
				   "wikis"
				   "wild-animal-initiative"
				   "wild-animal-welfare"
				   "william-macaskill"
				   "windfall-clause"
				   "women-s-health-and-welfare"
				   "working-at-ea-vs-non-ea-orgs"
				   "workplace-advocacy"
				   "workplace-groups"
				   "workshops-retreats-summits"
				   "worldview-diversity-in-effective-altruism"
				   "writing-advice"))

(defun ps/tlon-bae-insert-eawiki-slug ()
  "Insert an EA Wiki slug at point."
  (interactive)
  (insert (completing-read "URL: " ps/tlon-bae-eawiki-slugs)))

(defun ps/tlon-bae-finalize-translation ()
  "Finalize BAE translation.
With point in a buffer that contains a finished BAE translation,
perform the following operations:

1. `fill-region' on the entire buffer.
2. `save-buffer'.
3. stage changes in local repo.
4. commit changes in local repo.
5. push changes to remote repo."
  (interactive)
  (fill-region (point-min) (point-max))
  (save-buffer)
  (let* ((commit-summary-minus-filename "Translate ")
	 (commit-summary (concat
			  commit-summary-minus-filename
			  (truncate-string-to-width
			   (buffer-name)
			   (- git-commit-summary-max-length
			      (length commit-summary-minus-filename))))))
    (ps/magit-stage-commit-and-push commit-summary)
    (message "Commit `%s' pushed to remote BAE repo." commit-summary)))

(provide 'tlon)
;;; tlon.el ends here
