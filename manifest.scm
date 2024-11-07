(use-modules (guix packages)
             (gnu packages scheme))

(specifications->manifest
 '("chez-scheme"
   "sqlite"))
