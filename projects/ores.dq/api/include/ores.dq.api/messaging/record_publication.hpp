/** -*- mode: c++-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
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
#ifndef ORES_DQ_API_MESSAGING_RECORD_PUBLICATION_HPP
#define ORES_DQ_API_MESSAGING_RECORD_PUBLICATION_HPP

#include "ores.database/domain/context_fwd.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.logging/logger.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <cstdint>
#include <format>
#include <string>
#include <vector>

namespace ores::dq::messaging {

/**
 * @brief One thing a run said, in the shape the log column holds it.
 *
 * A warning is work the run chose not to do and an error is work it could not do.
 * The two are stated by the run and never inferred from its counts, because a run
 * that skipped a row and one that failed before touching any have the same
 * counts.
 */
struct publication_note {
    std::string severity;
    std::string text;
};

/**
 * @brief Appends one row to the publication run log.
 *
 * Every target service runs its own copy of the publish-from-dq step, and every
 * one of them records its runs through this function, so the log has one shape
 * and one writer's rules rather than one per component.
 *
 * The row states what the job needs and not what the caller happens to know: the
 * dataset's code, the table the rows went to and the person who asked are read
 * here, because the step runs under the tenant alone and the command names only
 * the dataset.
 *
 * A run is recorded whatever its outcome. A log of the successes alone answers
 * "when was this last published?" with an older run that did write something,
 * which is a different question from the one a person is asking.
 *
 * The write never fails the step: the rows are already published, and a log that
 * could not be written must not undo them.
 *
 * @param ctx The tenant context the step ran under.
 * @param dataset_id The dataset the run published.
 * @param instance_id The workflow instance, which names who asked for the run.
 * @param inserted,updated,skipped,deleted What the run wrote.
 * @param outcome =published=, =published_with_warnings= or =failed=.
 * @param notes What the run said, empty when it said nothing.
 * @param log The caller's logger, so the news lands where the step's does.
 */
inline void record_publication(const ores::database::context& ctx,
                               const std::string& dataset_id,
                               const std::string& instance_id,
                               const std::string& mode,
                               std::int64_t inserted,
                               std::int64_t updated,
                               std::int64_t skipped,
                               std::int64_t deleted,
                               const std::string& outcome,
                               const std::vector<publication_note>& notes,
                               ores::logging::logger_t& log) {
    std::string messages = "[";
    for (std::size_t i = 0; i < notes.size(); ++i) {
        if (i > 0)
            messages += ',';
        messages += "{\"severity\":" + ores::utility::codec::json::write(notes[i].severity) +
                    ",\"text\":" + ores::utility::codec::json::write(notes[i].text) + "}";
    }
    messages += "]";

    try {
        ores::database::query_builder::execute_parameterized_multi_column_query(
            ctx,
            "SELECT ores_dq_record_publication_fn($1::uuid, $2::uuid, $3, $4::bigint, "
            "$5::bigint, $6::bigint, $7::bigint, $8, $9::jsonb)",
            {dataset_id,
             instance_id,
             mode,
             std::to_string(inserted),
             std::to_string(updated),
             std::to_string(skipped),
             std::to_string(deleted),
             outcome,
             messages},
            log,
            "Recording a publication");
        ORES_LOG_SEV(log, ores::logging::info)
            << "Publication logged: dataset=" << dataset_id << " outcome=" << outcome;
    } catch (const std::exception& e) {
        // The rows are already published, so a log that cannot be written must
        // not undo them.
        ORES_LOG_SEV(log, ores::logging::warn) << "Publication not logged: " << e.what();
    }
}

} // namespace ores::dq::messaging

#endif
