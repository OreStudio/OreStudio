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
#ifndef ORES_MARKETDATA_SERVICE_TESTS_CURVE_PUBLISH_STEPS_HPP
#define ORES_MARKETDATA_SERVICE_TESTS_CURVE_PUBLISH_STEPS_HPP

#include "ores.database/domain/context_fwd.hpp"
#include "ores.inbox.api/domain/approval_request.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.marketdata.api/domain/curve_publish.hpp"
#include "ores.marketdata.core/repository/curve_publish_repository.hpp"
#include "ores.marketdata.service/app/derived_market_object_kind.hpp"
#include "ores.marketdata.service/app/publication_kind_codes.hpp"
#include "ores.marketdata.service/app/publication_lifecycle.hpp"
#include "ores.refdata.core/repository/ir_curve_bootstrap_config_repository.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <optional>
#include <stdexcept>
#include <string>
#include <vector>

namespace ores::marketdata::service::tests {

/**
 * @brief What a publish call asks to publish, read from the configuration itself.
 *
 * The lifecycle states a subject in its own shape; this suite's fixtures and
 * cases were written around the configuration, the party and the series, so the
 * steps below translate between the two and leave the cases as they were.
 */
struct publish_subject {
    boost::uuids::uuid party_id{};
    boost::uuids::uuid bootstrap_config_id{};
    boost::uuids::uuid output_series_id{};
    std::chrono::system_clock::time_point as_of{};
};

/// What the approval record says about one publish.
struct publish_verdict {
    bool approved = false;
    std::string state_code;
    std::string message;
    std::optional<ores::marketdata::domain::curve_publish> record;
};

/// The subject a configuration states as of an instant, through the one lifecycle.
inline publish_subject resolve_publish_subject(const ores::database::context& ctx,
                                               const std::string& bootstrap_config_id,
                                               std::chrono::system_clock::time_point at) {
    const auto resolved = ores::marketdata::service::app::resolve_publication_subject(
        ctx, ores::marketdata::service::app::derived_kind::curve, bootstrap_config_id, at);
    return {.party_id = resolved.party_id,
            .bootstrap_config_id = resolved.subject_id,
            .output_series_id = resolved.output_series_id,
            .as_of = resolved.as_of};
}

/**
 * @brief The verdict on one publish, with the record it authorises.
 *
 * The kind answers in the lifecycle's own facts; a case that wants to hand the
 * record to the apply reads it back by the request and the configuration, which
 * is the row the request names.
 */
inline publish_verdict verdict_on_publish(const ores::database::context& ctx,
                                          const std::string& request_id,
                                          const publish_subject& attempted) {
    const auto& kind = ores::marketdata::service::app::publication_kind_of(
        ores::marketdata::service::app::derived_kind::curve);
    const auto verdict = kind.verdict_on_publish(
        ctx,
        request_id,
        {.kind = ores::marketdata::service::app::derived_kind::curve,
         .party_id = attempted.party_id,
         .subject_id = attempted.bootstrap_config_id,
         .subject_key = boost::uuids::to_string(attempted.bootstrap_config_id),
         .output_series_id = attempted.output_series_id,
         .as_of = attempted.as_of});
    publish_verdict out;
    out.approved = verdict.approved;
    out.state_code = verdict.state_code;
    out.message = verdict.message;
    if (verdict.approved) {
        const auto stored =
            ores::marketdata::repository::curve_publish_repository{}
                .read_latest_by_request_and_config(
                    ctx, request_id, boost::uuids::to_string(attempted.bootstrap_config_id));
        out.record = stored;
    }
    return out;
}

/// The request a raise wrote, read back so a case can decide it.
inline ores::inbox::domain::approval_request read_request(const ores::database::context& ctx,
                                                          const std::string& request_id) {
    const auto request = ores::inbox::service::approval_lifecycle(ctx).request(request_id);
    if (!request)
        throw std::runtime_error("The request that was just raised cannot be read back.");
    return *request;
}

/// One raise of one curve, through the one lifecycle.
inline ores::inbox::domain::approval_request raise_publish_request(
    const ores::database::context& ctx, const publish_subject& subject, const std::string& reason) {
    const auto raised = ores::marketdata::service::app::raise_publication(
        ctx,
        ores::marketdata::service::app::derived_kind::curve,
        {{.subject_key = boost::uuids::to_string(subject.bootstrap_config_id),
          .as_of = subject.as_of}},
        reason,
        subject.party_id,
        ores::marketdata::service::app::set_raise::whole_set);
    return read_request(ctx, raised.request_id);
}

/// One raise of a set of curves, whole or not at all.
inline ores::inbox::domain::approval_request
raise_publish_request_set(const ores::database::context& ctx,
                          const std::vector<publish_subject>& subjects,
                          const std::string& reason) {
    std::vector<ores::marketdata::service::app::publication_request> requests;
    requests.reserve(subjects.size());
    for (const auto& subject : subjects)
        requests.push_back({.subject_key = boost::uuids::to_string(subject.bootstrap_config_id),
                            .as_of = subject.as_of});
    const auto raised = ores::marketdata::service::app::raise_publication(
        ctx,
        ores::marketdata::service::app::derived_kind::curve,
        requests,
        reason,
        subjects.front().party_id,
        ores::marketdata::service::app::set_raise::whole_set);
    return read_request(ctx, raised.request_id);
}

/// The apply of an approved request, through the one lifecycle.
inline ores::marketdata::domain::curve_publish
record_publication(const ores::database::context& ctx,
                   ores::marketdata::domain::curve_publish record,
                   const std::string& publisher) {
    const auto applied = ores::marketdata::service::app::apply_publication(
        ctx,
        ores::marketdata::service::app::derived_kind::curve,
        boost::uuids::to_string(record.request_id),
        {.kind = ores::marketdata::service::app::derived_kind::curve,
         .party_id = record.party_id,
         .subject_id = record.bootstrap_config_id,
         .subject_key = boost::uuids::to_string(record.bootstrap_config_id),
         .output_series_id = record.output_series_id,
         .as_of = record.as_of},
        publisher);
    if (applied.status != ores::marketdata::service::app::publication_status::published)
        throw std::invalid_argument(applied.message);
    const auto stored = ores::marketdata::repository::curve_publish_repository{}.read_latest(
        ctx, boost::uuids::to_string(record.id));
    if (stored.empty())
        throw std::runtime_error("The published set was written but cannot be read back.");
    return stored.front();
}

}

#endif
