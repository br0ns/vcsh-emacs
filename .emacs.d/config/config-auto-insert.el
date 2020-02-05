(auto-insert-mode +1)
(setq-default auto-insert-query nil)

;; '(define-auto-insert
;;    ;; C Program
;;    '("\\.c$" . "C Program")
;;    '("# include <stdlib.h>\n\n"
;;      "# include <unistd.h>\n"
;;      "# include <stdio.h>\n"
;;      "\n"
;;      "# include \""
;;      (file-name-nondirectory
;;       (file-name-sans-extension
;;        buffer-file-name))
;;      ".h\"\n"
;;      )
;;    )

;;   '("\\.\\(CC?\\|cc\\|cxx\\|cpp\\|c++\\)\\'" . "C++ skeleton")
;;      '("Short description: "
;;        "/*" \n
;;        (file-name-nondirectory (buffer-file-name))
;;        " -- " str \n
;;        " */" > \n \n
;;        "#include <iostream>" \n \n
;;        "using namespace std;" \n \n
;;        "main()" \n
;;        "{" \n
;;        > _ \n
;;        "}" > \n)))

(add-to-list 'auto-insert-alist
             '(("\\.c$" . "C program")
               nil
               "#include <stdlib.h>\n"
               "#include <unistd.h>\n"
               "#include <stdio.h>\n"
               "\n"
               > _
               "#include \""
               (file-name-nondirectory
                (file-name-sans-extension
                 buffer-file-name))
               ".h\"\n"
               ))

(add-to-list 'auto-insert-alist
             '(("\\.h$" . "C header")
               (upcase (concat
                        "_"
                        (file-name-nondirectory
                         (substring buffer-file-name 0 (match-beginning 0)))
                        "_H"))
               "#ifndef " str "\n"
               "#define " str "\n\n"
               > _
               "\n\n#endif /* " str " */\n"
               ))

(add-to-list 'auto-insert-alist
             '(("\\.py$" . "Python program")
               nil
               "#!/usr/bin/env python2.7\n"
               "import os\n"
               "import sys\n"
               "\n"
               > _
               ))

(add-to-list 'auto-insert-alist
             '(("\\.pypy$" . "Python program (pypy)")
               nil
               "#!/usr/bin/env pypy\n"
               "import os\n"
               "import sys\n"
               "\n"
               > _
               ))

(provide 'config-auto-insert)
