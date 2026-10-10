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

private:
    ores::database::context ctx_;
};

}

#endif
