;;; elfetch.el --- Display information about Emacs   -*- lexical-binding: t; -*-

;; Copyright (C) 2021 aabm

;; Author: aabm <aabm@disroot.org>
;; Keywords: games

;; This program is free software; you can redistribute it and/or modify
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

;; Elfetch is a system information program for Emacs, inspired by Unix
;; programs such as `neofetch' and `pfetch'.

;;; Code:

(defun elfetch ()
  (interactive)
  (switch-to-buffer "*elfetch*")
  (insert "    _-`````-,           ,- '- .
  .'   .- - |          | - -.  `.
 /.'  /                     `.   \
:/   :      _...   ..._      ``   :
::   :     /._ .`:'_.._\\.    ||   :
::    `._ ./  ,`  :    \ . _.''   .
`:.      /   |  -.  \\-. \\_      /
  \\:._ _/  .'   .@)  \\@) ` `\ ,.'
     _/,--'       .- .\\,-.`--`.
       ,'/''     (( \ `  )
        /'/'  \    `-'  (
         '/''  `._,-----'
          ''/'    .,---'
           ''/'      ;:
             ''/''  ''/
               ''/''/''
                 '/'/'
                  `;
aabm@deck 
--------- 
OS: Arch Linux 
Kernel: 5.11.15-arch1-2 
Uptime: 1h 14m 
Packages: 986 (pacman) 
Shell: bash 5.1.4 
WM: bspwm 
CPU: Intel i5-8265U (8) @ 3.900GHz 
Memory: 1040MiB / 3789MiB 

                        
")
  (read-only-mode))


(provide 'elfetch)
;;; elfetch.el ends here
