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
#include "ores.inbox.api/domain/approval_part.hpp"
#include "ores.inbox.api/domain/approval_request.hpp"
#include "ores.inbox.core/export.hpp"
#include "ores.logging/make_logger.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
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
 * @brief A request the sweep closed because nobody answered it.
 */
struct expired_request {
    std::string request_id;
    std::string tenant_id;
    std::string kind_code;
    std::string requested_by;
};

/**
 * @brief A request whose deadline is close, and the people who may answer it.
 */
struct expiring_request {
    std::string request_id;
    std::string tenant_id;
    std::string kind_code;
    std::string requested_by;
    std::string expires_at;
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
                                   const boost::uuids::uuid& requested_by,
                                   const std::vector<std::string>& part_codes = {});

    /**
     * @brief The parts a request needs, with the order each answers in.
     *
     * Empty for a request of a kind that names one decider permission and a
     * count. Lowest answer order first, then by display order.
     */
    std::vector<domain::approval_part> parts_of(const std::string& request_id);

    /**
     * @brief The parts a request still waits on whose turn has come.
     *
     * A part is open when it has not approved and every part of an earlier
     * answer order has.
     */
    std::vector<domain::approval_part> open_parts_of(const std::string& request_id);

    /**
     * @brief A part, read from the system tenant's catalogue.
     */
    std::optional<domain::approval_part> part(const std::string& code);

    /**
     * @brief Writes a decision and moves the request to the state the
     * decisions reach, in one statement.
     */
    decision_result decide(const std::string& request_id,
                           int version,
                           const std::string& decision_code,
                           const boost::uuids::uuid& decided_by,
                           const std::string& comment,
                           const std::string& part_code = "");

    /**
     * @brief The open requests of the given kinds not raised by an account,
     * oldest first.
     */
    request_page queue(const std::vector<std::string>& kind_codes,
                       const boost::uuids::uuid& excluding,
                       int offset,
                       int limit);

    /**
     * @brief The requests of the given kinds, not raised by an account, that
     * were answered within a window, newest answer first.
     *
     * A request leaves the open queue the moment it is answered, so the person
     * who answered it has nothing to look at when a notice links back to what
     * happened. This is the tail that keeps it in view.
     *
     * The answer is the moment the request's current version was written,
     * which for an answered request is the moment of the answer. A window of
     * zero answers nothing.
     */
    std::vector<domain::approval_request>
    recently_answered(const std::vector<std::string>& kind_codes,
                      const boost::uuids::uuid& excluding,
                      std::chrono::seconds window);

    /**
     * @brief The requests an account raised, newest first.
     */
    request_page raised_by(const boost::uuids::uuid& account_id, int offset, int limit);

    /**
     * @brief Closes every open request past its kind's deadline, across every
     * tenant, and tells each person who asked.
     *
     * A request nobody looks at is the case a queue rots on, so this runs on
     * its own rather than waiting for a decider to reach for one. It is
     * idempotent: a request it already closed is no longer open, so a repeated
     * run changes nothing.
     */
    std::vector<expired_request> expire_overdue();

    /**
     * @brief Warns the deciders of every open request whose deadline falls
     * inside a window, across every tenant.
     *
     * The other half of the deadline: the sweep that closes what nobody
     * answered keeps the queue moving, and this gives the person who may answer
     * the chance the deadline was there to give. A request is warned about
     * once, and the notice already raised is what says so.
     */
    std::vector<expiring_request> remind_expiring(std::chrono::seconds window);

private:
    /**
     * @brief Tells the person who asked that their request ran out of time.
     *
     * Telling is never the closing: a request closed and untold is better than
     * one told and not closed, so a failure here is logged and swallowed.
     */
    void tell_expired(const expired_request& expired);

    /**
     * @brief Tells the people who may answer that a request is close to its
     * deadline.
     *
     * Warning is never the sweep: a request warned about and not warned about
     * again is better than one the sweep failed on. A request with nobody left
     * to warn is not a failure either, and a failure here is logged and
     * swallowed.
     */
    void tell_expiring(const expiring_request& expiring);

    ores::database::context ctx_;
};

}

#endif
