(defmodule rd_heartbeat
  (behaviour gen_server)
  ;; API
  (export (start_link 1) (start_link 0))
  ;; gen_server callbacks
  (export (init 1)
          (handle_call 3) (handle_cast 2) (handle_info 2)
          (terminate 2)
          (code_change 3)))

(defun SERVER () (MODULE))

(defrecord state frequency)


;;;===================================================================
;;; API
;;;===================================================================

(defun start_link (frequency)
  "Start the server."
  (gen_server:start_link `#(local ,(SERVER)) (MODULE) `[,frequency] '[]))

(defun start_link ()
  "Equivalent to `(start_link 0)`, which indicates no heartbeating."
  (let ((`#(ok ,frequency) (rd_util:get_env 'heartbeat_frequency 60000)))
    (start_link frequency)))


;;;===================================================================
;;; gen_server callbacks
;;;===================================================================

(defun init
  "Initiate the server."
  ([`(,frequency)]
   (let (('ok (resource_discovery:contact_nodes)))
     `#(ok ,(make-state frequency frequency) frequency))))

(defun handle_call (_request _from state)
  "Handle call messages."
  `#(reply ok ,state))

(defun handle_cast (_msg state)
  "Handle cast messages."
  `#(noreply ,state))

(defun handle_info
  "Handle all non call/cast messages."
  (['timeout (= state (match-state frequency 0))]
   `#(stop normal ,state))
  (['timeout (= state (match-state frequency frequency))]
   (resource_discovery:contact_nodes)
   (resource_discovery:trade_resources)
   ;; Wait for approximately the frequency with a random factor
   `#(noreply ,state ,(+ (random:uniform (div frequency 2))
                         (div frequency 2)
                         (div frequency 3)))))

(defun terminate (reason _state)
  "Shut down the the server."
  (error_logger:info_msg "Stopping resource discovery heartbeat: ~p" `[,reason])
  'ok)

(defun code_change (_old-vsn state _extra)
  "Convert process state when code is changed."
  `#(ok ,state))
