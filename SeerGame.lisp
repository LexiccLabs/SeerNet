(mod

    (SYNTHETIC_PUBLIC_KEY original_public_key delegated_puzzle solution)

    ; "assert" is a macro that wraps repeated instances of "if"
    ; usage: (assert A0 A1 ... An R)
    ; all of A0, A1, ... An must evaluate to non-null, or an exception is raised
    ; return the last item (if we get that far)

    (defmacro assert (items)
        (if (r items)
            (list if (f items) (c assert (r items)) (q . (x)))
            (f items)
        )
    )

    (include condition_codes.clvm)
    (include sha256tree1.clvm)

    ; "is_hidden_puzzle_correct" returns true iff the hidden puzzle is correctly encoded

    (defun-inline is_hidden_puzzle_correct (SYNTHETIC_PUBLIC_KEY original_public_key delegated_puzzle)
      (=
          SYNTHETIC_PUBLIC_KEY
          (point_add
              original_public_key
              (pubkey_for_exp (sha256 original_public_key (sha256tree1 delegated_puzzle)))
          )
      )
    )

    ; "possibly_prepend_aggsig" is the main entry point

    (defun-inline possibly_prepend_aggsig (SYNTHETIC_PUBLIC_KEY original_public_key delegated_puzzle conditions)
      (if original_public_key
          (assert
              (is_hidden_puzzle_correct SYNTHETIC_PUBLIC_KEY original_public_key delegated_puzzle)
              conditions
          )
          (c (list AGG_SIG_ME SYNTHETIC_PUBLIC_KEY (sha256tree1 delegated_puzzle)) conditions)
      )
    )

    ; main entry point

    (possibly_prepend_aggsig
        SYNTHETIC_PUBLIC_KEY original_public_key delegated_puzzle
        (a delegated_puzzle solution))
)
