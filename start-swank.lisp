(ql:quickload :swank)
(swank:create-server :dont-close t)
(loop (sleep 1))
