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
#ifndef ORES_REFDATA_SERVICE_SERVICE_COUNTERPARTY_PROPOSAL_HPP
#define ORES_REFDATA_SERVICE_SERVICE_COUNTERPARTY_PROPOSAL_HPP

#include "ores.database/domain/context.hpp"
#include "ores.inbox.api/domain/approval_request.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/counterparty_change.hpp"
#include "ores.refdata.api/domain/counterparty_identifier_change.hpp"
#include "ores.refdata.api/domain/party_counterparty_change.hpp"
#include "ores.refdata.service/export.hpp"
#include "ores.refdata.service/service/book_proposal.hpp"
#include <cstddef>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief The proposed writes of one request, by the kind of row they change.
 *
 * The kinds apply in this order: the counterparties, then their identifiers,
 * then the links, so a new counterparty exists before the rows that name it.
 * The lines are numbered across the kinds in the same order.
 */
struct counterparty_lines {
    std::vector<domain::counterparty_change> counterparties;
    std::vector<domain::counterparty_identifier_change> identifiers;
    std::vector<domain::party_counterparty_change> links;

    [[nodiscard]] std::size_t size() const {
        return counterparties.size() + identifiers.size() + links.size();
    }
};

/**
 * @brief What the real write said about one proposed line.
 *
 * The refusal is empty when the write would stand. The columns are the ones the
 * line changes, and the parts are the ones the policy names for the line alone.
 */
struct counterparty_line_outcome {
    int line_no = 0;
    std::string entity_type;
    std::string operation;
    std::string subject_id;
    std::vector<std::string> columns;
    std::string refusal;
    std::vector<std::string> part_codes;
};

/**
 * @brief The answer to a preview: one outcome per line, and the parts the
 * policy needs for the lines together.
 */
struct counterparty_preview {
    std::vector<counterparty_line_outcome> lines;
    std::vector<std::string> part_codes;

    /**
     * @brief Whether the real write refused any line.
     */
    [[nodiscard]] bool refused() const;
};

/**
 * @brief One stored line as a decider reads it.
 */
struct counterparty_line_delta {
    int line_no = 0;
    std::string entity_type;
    std::string operation;
    std::string subject_id;
    int base_version = 0;
    bool applied = false;
    std::vector<field_delta> fields;
};

/**
 * @brief What a raise came to.
 *
 * The request is set only when it was raised. Otherwise the message says why
 * not, and the preview holds the refusals.
 */
struct counterparty_proposal {
    std::optional<ores::inbox::domain::approval_request> request;
    counterparty_preview preview;
    std::string message;
};

/**
 * @brief What an apply came to.
 *
 * An apply either writes every line and marks each applied, or writes none. A
 * refusal is permanent: the request has moved to apply_failed. A transient
 * failure is the infrastructure failing: nothing was written, the request stays
 * approved, and the apply may be run again.
 */
struct counterparty_apply {
    bool applied = false;
    bool transient = false;
    int lines_applied = 0;
    int failed_line = 0;
    std::string refusal;
};

/**
 * @brief Proposes changes to counterparties as typed lines held by an approval
 * request.
 *
 * A request holds lines of three kinds: a counterparty, an identifier of a
 * counterparty, and the link that lets a party trade with a counterparty. The
 * preview runs the real service for each line inside a transaction it never
 * commits, with a savepoint per line, so a refusal comes back at once and
 * nothing is written, published or sent. The raise previews, then writes the
 * request, its parts and its lines in one transaction.
 */
class ORES_REFDATA_SERVICE_EXPORT counterparty_proposal_service {
private:
    [[nodiscard]] static auto& lg() {
        static auto instance =
            ores::logging::make_logger("ores.refdata.service.counterparty_proposal");
        return instance;
    }

public:
    explicit counterparty_proposal_service(ores::database::context ctx);

    /**
     * @brief Runs every line through the real write and returns its outcome.
     *
     * Leaves the live tables and the pending tables as they were.
     */
    counterparty_preview preview(const counterparty_lines& lines);

    /**
     * @brief Previews the lines and, when none is refused and the policy names
     * a part, raises the request that holds them.
     *
     * Tells the deciders of the parts that can answer now, after the request
     * is written.
     */
    counterparty_proposal raise(const counterparty_lines& lines, const std::string& reason);

    /**
     * @brief Runs the stored lines of a request through the real write again
     * and writes nothing.
     */
    counterparty_preview recheck(const std::string& request_id);

    /**
     * @brief The stored lines of a request, each beside the row it changes.
     *
     * Reads only. A put against a row lists its columns against the row at the
     * version the maker read, so a line that has applied still shows what it
     * changed. A link has no columns of its own and no version to read, so its
     * fields are the two halves, marked changed on a put; a reader should take a
     * link line as made or removed whole and not read changed on its fields.
     */
    std::vector<counterparty_line_delta> deltas(const std::string& request_id);

    /**
     * @brief Applies the stored lines of an approved request in one
     * transaction.
     *
     * Each line goes through the real write, with the request as the reason,
     * and is marked applied in the same transaction. The removals go first, the
     * rows that name another row before the row they name, and the writes second,
     * the row named before the rows that name it, so a request can mix both.
     * The first refusal rolls everything back and moves the request to
     * apply_failed. A failure of the infrastructure rolls back and returns
     * transient.
     */
    counterparty_apply apply(const std::string& request_id);

    /**
     * @brief The most lines a request may hold.
     */
    static constexpr std::size_t max_lines = 1000;

private:
    ores::database::context ctx_;
};

}

#endif
