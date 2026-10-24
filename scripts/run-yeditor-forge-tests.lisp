;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Enterprise
;;;; Proprietary and Confidential — Yeditor Agentic & Forge Suite (see LICENSE-ENTERPRISE).
(load ".qlot/setup.lisp")
(ql:quickload "yeditor-enterprise-tests")
(dolist (test '(yeditor-enterprise-forge-tests::test-forge-promotes-a-reversible-component
                yeditor-enterprise-forge-tests::test-forge-rejects-unsafe-components
                yeditor-enterprise-forge-tests::rejected-duplicate-preserves-the-existing-component
                yeditor-enterprise-forge-tests::reject-hidden-code-in-lambda-lists
                yeditor-enterprise-forge-tests::component-names-cannot-be-file-paths
                yeditor-enterprise-forge-tests::metadata-is-data-and-promotion-never-overwrites
                yeditor-enterprise-forge-tests::generation-is-async-and-promotion-runs-on-ui
                yeditor-enterprise-forge-tests::forge-palette-restores-100-times))
  (unless (rove:run-test test) (uiop:quit 1)))
(uiop:quit 0)
