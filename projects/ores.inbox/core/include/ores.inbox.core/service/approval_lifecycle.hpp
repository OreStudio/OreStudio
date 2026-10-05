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
#ifndef ORES_INBOX_CORE_SERVICE_APPROVAL_LIFECYCLE_HPP
#define ORES_INBOX_CORE_SERVICE_APPROVAL_LIFECYCLE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.inbox.api/domain/approval_kind.hpp"
#include "ores.inbox.api/domain/approval_request.hpp"
#include "ores.inbox.core/export.hpp"
#include "ores.logging/make_logger.hpp"
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <vector>

namespace ores::inbox::service {

/**
 * @brief What a decision on a request came to.
 *
 * The outcome is one of the protocol's outcome names: ok, missing, conflict
 * or invalid. The state and version are the request's afterwards, so a screen
 * shows where the request now stands even when the decision was refused.
 */
struct decision_result {
    std::string outcome;
    std::string message;
    std::string state_code;
    int version = 0;
};

/**
 * @brief One page of requests and the size of the whole list.
 */
struct request_page {
    std::vector<domain::approval_request> requests;
    int total = 0;
};

/**
 * @brief The approval request lifecycle: raising, deciding and the two reads
 * a person works from.
 *
 * Deciding runs in the database as one statement, so a decision and the
 * state it moves the request to are written together. Permission checks are
 * the caller's: this class trusts that the decider may decide.
 */
class ORES_INBOX_CORE_EXPORT approval_lifecycle {
private:
    [[nodiscard]] static auto& lg() {
        static auto instance = ores::logging::make_logger("ores.inbox.service.approval_lifecycle");
        return instance;
    }

public:
    explicit approval_lifecycle(ores::database::context ctx);

    /**
     * @brief The account the context's actor signs in as, if any.
     */
    std::optional<boost::uuids::uuid> actor_account_id();

    /**
     * @brief A kind of request, read from the system tenant's catalogue.
     */
    std::optional<domain::approval_kind> kind(const std::string& code);

    /**
     * @brief Every kind of request.
     */
    std::vector<domain::approval_kind> kinds();

    /**
     * @brief The current version of a request, if it exists.
     */
    std::optional<domain::approval_request> request(const std::string& id);

    /**
     * @brief Raises a waiting request of a kind for an account.
     *
     * The request expires when the kind says, or never.
     */
    domain::approval_request raise(const domain::approval_kind& kind,
                                   const std::string& reason,
                                   const boost::uuids::uuid& requested_by);

    /**
     * @brief Writes a decision and moves the request to the state the
     * decisions reach, in one statement.
     */
    decision_result decide(const std::string& request_id,
                           int version,
                           const std::string& decision_code,
                           const boost::uuids::uuid& decided_by,
                           const std::string& comment);

    /**
     * @brief The open requests of the given kinds not raised by an account,
     * oldest first.
     */
    request_page queue(const std::vector<std::string>& kind_codes,
                       const boost::uuids::uuid& excluding,
                       int offset,
                       int limit);

    /**
     * @brief The requests an account raised, newest first.
     */
    request_page raised_by(const boost::uuids::uuid& account_id, int offset, int limit);

private:
    ores::database::context ctx_;
};

}

#endif
