/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_QT_TEST_SCENARIO_RESULTS_WRITER_HPP
#define ORES_QT_TEST_SCENARIO_RESULTS_WRITER_HPP

#include "ores.qt/EnvironmentMetadata.hpp"
#include "ores.qt/export.hpp"
#include <QString>
#include <vector>

namespace ores::qt {

struct step_result final {
    QString step_text;
    bool passed = false;

    /**
     * @brief Which client instance this step ran on (e.g. "blue",
     * "red"), for a multi-client scenario (see the =Clients= field in
     * =Scenario Info=). Empty for the common single-client case — the
     * rendered table only gains a =Client= column when at least one
     * step in the run has this set.
     */
    QString client;
};

/**
 * @brief Everything the QA Validation Runner panel collects from the
 * tester for one run.
 */
struct scenario_result final {
    QString status; // "PASSED" or "FAILED"
    std::vector<step_result> steps;
    QString notes;
};

/**
 * @brief Rewrite a =test_scenario= doc's =* Results= and =* Notes=
 * sections in place — the *only* two sections this touches; every
 * other line in the file (Scenario Info, Steps, frontmatter, ...) is
 * left byte-identical.
 *
 * This is a targeted text splice, not a round-trip
 * parse-then-re-serialize: it finds each heading's line span (up to
 * the next heading at the same or a shallower level, or EOF) and
 * replaces just that span. Deliberately not a capability
 * =ores.orgmode='s reader needs to support generically — a generic
 * writer would need to preserve every byte of everything it *doesn't*
 * touch, which is a much larger problem than this one targeted
 * rewrite.
 *
 * @param path Path to the =test_scenario= =.org= file.
 * @param result What to write into =* Results=/=* Notes=.
 * @param environment Branch/commit/worktree, normally from
 * =capture_environment_metadata()=.
 * @return true on success; false if the file couldn't be read/written
 * or didn't contain a =* Results= heading to rewrite.
 */
ORES_QT_API bool write_scenario_results(const QString& path,
                                        const scenario_result& result,
                                        const environment_metadata& environment);

}

#endif
