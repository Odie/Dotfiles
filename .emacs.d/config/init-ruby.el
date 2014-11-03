(use-package ruby-mode
	:mode ( ("\\.rake\\'" . ruby-mode)
					("Rakefile\\'" . ruby-mode)
					("\\.gemspec\\'" . ruby-mode)
					("\\.ru\\'" . ruby-mode)
					("Gemfile\\'" . ruby-mode)
					("Guardfile\\'" . ruby-mode)
					("Capfile\\'" . ruby-mode)
					("\\.thor\\'" . ruby-mode)
					("\\.rabl\\'" . ruby-mode)
					("Thorfile\\'" . ruby-mode)
					("Vagrantfile\\'" . ruby-mode)
					("\\.jbuilder\\'" . ruby-mode)
					("Podfile\\'" . ruby-mode)
					("\\.podspec\\'" . ruby-mode)
					("Puppetfile\\'" . ruby-mode)
					("Berksfile\\'" . ruby-mode)
					("Appraisals\\'" . ruby-mode))

	:config
	(progn
	  (smart-tabs-advice ruby-indent-line ruby-indent-level)
	  (setq ruby-indent-tabs-mode t)
	  ))
