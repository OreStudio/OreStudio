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
#ifndef ORES_MARKETDATA_SERVICE_APP_CURVE_PUBLISH_GATE_HPP
#define ORES_MARKETDATA_SERVICE_APP_CURVE_PUBLISH_GATE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.inbox.api/domain/approval_request.hpp"
#include "ores.marketdata.api/domain/curve_publish.hpp"
#include "ores.marketdata.service/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::marketdata::service::app {

/**
 * @brief The kind of approval request that permits a curve publish.
 *
 * A publish is a request and not a save: the trader who owns the curve asks,
 * a person holding the kind's decider permission -- Market Risk -- answers, and
 * only an approved request publishes. The kind is seeded in
 * projects/ores.sql/populate/inbox/inbox_approval_kinds_populate.sql.
 */
inline constexpr std::string_view curve_publish_kind_code = "marketdata.curve_publish";

/**
 * @brief What a publish call asks to publish, read from the configuration itself.
 *
 * The caller states the configuration and the as-of; the party and the series
 * are the configuration's to say, so a caller cannot publish one party's curve
 * under another's name.
 */
struct publish_subject {
    boost::uuids::uuid party_id{};
    boost::uuids::uuid bootstrap_config_id{};
    boost::uuids::uuid output_series_id{};
    std::chrono::system_clock::time_point as_of{};
};

/**
 * @brief What the approval record says about one publish.
 *
 * approved says the publish may go ahead. state_code is the request's state as
 * the record holds it, so a caller reads waiting or refused rather than
 * guessing, and it is empty when no request was named at all. record is the
 * subject the request names, present as soon as one was found. message says why
 * a publish may not go ahead, in the words the person reads.
 */
struct publish_verdict {
    bool approved = false;
    std::string state_code;
    std::string message;
    std::optional<ores::marketdata::domain::curve_publish> record;
};

/**
 * @brief The permission the kind names for raising a request of it, or empty.
 *
 * The maker's half of the pair is the kind's, so the operation asks the kind and
 * never a code written here: a tenant that moves the maker moves the gate with
 * it. Empty means the kind names none, and then the raise is the component's own
 * to gate, as every kind seeded before the column was behaves.
 *
 * @throws std::runtime_error when the kind is not seeded.
 */
ORES_MARKETDATA_SERVICE_EXPORT std::string raise_permission_of(const ores::database::context& ctx,
                                                               std::string_view kind_code);

/**
 * @brief Reads the subject a publish call names out of the configuration.
 *
 * The configuration states the party it belongs to and the series its output is
 * written to, so both are read rather than trusted from the caller. An empty
 * identifier, an unknown configuration, or an as-of at the epoch is refused with
 * the reason: a publish that names no subject authorises nothing.
 *
 * @throws std::invalid_argument when the call names no subject.
 */
ORES_MARKETDATA_SERVICE_EXPORT publish_subject
resolve_publish_subject(const ores::database::context& ctx,
                        const std::string& bootstrap_config_id,
                        std::chrono::system_clock::time_point as_of);

/**
 * @brief Raises the request a publish waits on, and writes the subject it names.
 *
 * The request is raised for the signed-in person, so four-eyes holds at the
 * record: whoever decides it must be somebody else. The subject is written as
 * the kind's detail row in the same act, so a request that names nothing cannot
 * exist, and the request identifier is that row's natural key, so a request
 * authorises one publish and no second row can name it.
 *
 * @throws std::invalid_argument when the kind, the account or the subject is
 * missing.
 * @throws std::runtime_error when the store refuses the detail row.
 */
ORES_MARKETDATA_SERVICE_EXPORT ores::inbox::domain::approval_request raise_publish_request(
    const ores::database::context& ctx, const publish_subject& subject, const std::string& reason);

/**
 * @brief Asks the approval record whether a publish may go ahead.
 *
 * The request must be a @c marketdata.curve_publish request of the caller's
 * tenant, it must be approved, the subject it names must be the subject the
 * caller is publishing, and it must not have been applied before. Everything
 * else is a refusal that names both subjects, or the state the request is in, so
 * a person who names the wrong request or publishes another curve is told which
 * curve the request is about.
 */
ORES_MARKETDATA_SERVICE_EXPORT publish_verdict
verdict_on_publish(const ores::database::context& ctx,
                   const std::string& request_id,
                   const publish_subject& attempted);

/**
 * @brief Records the act: the request that authorised it, the publisher and the time.
 *
 * The row is written with the version it was read at, so a second apply of the
 * same request -- by this caller or another -- is refused by the store rather
 * than by a check that could race. The history keeps the unapplied subject
 * beside the applied one, and the request identifier on every version is what
 * says which decision authorised the published set.
 *
 * @param record The subject, as the verdict read it.
 * @param publisher The username of the account that applied the request.
 * @return The record as written, so the caller answers with the instant and the
 * account the store holds rather than with its own clock.
 * @throws std::invalid_argument when no publisher is named.
 */
ORES_MARKETDATA_SERVICE_EXPORT ores::marketdata::domain::curve_publish
record_publication(const ores::database::context& ctx,
                   ores::marketdata::domain::curve_publish record,
                   const std::string& publisher);

}

#endif
