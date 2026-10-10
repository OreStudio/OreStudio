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
#ifndef ORES_REFDATA_SERVICE_SERVICE_BOOK_PROPOSAL_HPP
#define ORES_REFDATA_SERVICE_SERVICE_BOOK_PROPOSAL_HPP

#include "ores.database/domain/context.hpp"
#include "ores.inbox.api/domain/approval_request.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/book_change.hpp"
#include "ores.refdata.service/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief What the real write said about one proposed line.
 *
 * The refusal is empty when the write would stand. The columns are the ones
 * the line changes, which is what the policy reads to name the parts.
 */
struct line_outcome {
    int line_no = 0;
    std::string operation;
    boost::uuids::uuid entity_id;
    std::vector<std::string> columns;
    std::string refusal;
};

/**
 * @brief The answer to a preview: one outcome per line, and the parts the
 * policy needs for the lines together.
 */
struct book_preview {
    std::vector<line_outcome> lines;
    std::vector<std::string> part_codes;

    /**
     * @brief Whether the real write refused any line.
     */
    [[nodiscard]] bool refused() const;
};

/**
 * @brief What a raise came to.
 *
 * The request is set only when it was raised. Otherwise the message says why
 * not, and the preview holds the refusals.
 */
struct book_proposal {
    std::optional<ores::inbox::domain::approval_request> request;
    book_preview preview;
    std::string message;
};

/**
 * @brief What an apply came to.
 *
 * An apply either writes every line and marks each applied, or writes none.
 * A refusal is permanent: the real write gave an answer that cannot change, and
 * the request has moved to apply_failed. A transient failure is the
 * infrastructure failing, such as a lost connection: nothing was written, the
 * request stays approved, and the apply may be run again.
 */
struct book_apply {
    bool applied = false;
    bool transient = false;
    int lines_applied = 0;
    int failed_line = 0;
    std::string refusal;
};

/**
 * @brief Whether an error's text names the infrastructure failing and not the
 * database refusing a write.
 */
ORES_REFDATA_SERVICE_EXPORT bool is_infrastructure_failure(const std::string& what);

/**
 * @brief Proposes changes to books as typed lines held by an approval request.
 *
 * A line is a pending book change: the book's own columns, the operation and
 * the version the maker read. The preview runs the real book service for
 * each line inside a transaction it never commits, with a savepoint per line,
 * so a refusal comes back at once and nothing is written, published or sent.
 * The raise previews, then writes the request, its parts and its lines in one
 * transaction.
 */
class ORES_REFDATA_SERVICE_EXPORT book_proposal_service {
private:
    [[nodiscard]] static auto& lg() {
        static auto instance = ores::logging::make_logger("ores.refdata.service.book_proposal");
        return instance;
    }

public:
    explicit book_proposal_service(ores::database::context ctx);

    /**
     * @brief Runs every line through the real write and returns its outcome.
     *
     * Leaves the live table and the pending table as they were.
     */
    book_preview preview(const std::vector<ores::refdata::domain::book_change>& lines);

    /**
     * @brief Previews the lines and, when none is refused and the policy
     * names a part, raises the request that holds them.
     *
     * Tells the deciders of the parts that can answer now, after the request
     * is written.
     */
    book_proposal raise(const std::vector<ores::refdata::domain::book_change>& lines,
                        const std::string& reason);

    /**
     * @brief Runs the stored lines of a request through the real write again
     * and writes nothing.
     *
     * The live book may have moved since the raise, so this is the check the
     * apply repeats. It reads every line of the request, applied or not.
     */
    book_preview recheck(const std::string& request_id);

    /**
     * @brief Applies the stored lines of an approved request in one transaction.
     *
     * Each line goes through the real write, with the request as the reason, and
     * is marked applied in the same transaction, so a line applies once and a
     * second call finds nothing to do. The first refusal rolls everything back,
     * moves the request to apply_failed with the line and the reason, and
     * returns it. A failure of the infrastructure rolls back and returns
     * transient, leaving the request approved.
     */
    book_apply apply(const std::string& request_id);

private:
    ores::database::context ctx_;
};

}

#endif
