(ql:quickload :slynk)
(slynk:create-server :dont-close t)
(loop (sleep 1))
