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
#include "ores.reporting.service/service/cut_take.hpp"
#include "ores.database/repository/unit_of_work.hpp"
#include "ores.inbox.core/service/approval_announcer.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.reporting.core/repository/cut_approval_detail_repository.hpp"
#include "ores.reporting.core/repository/cut_gap_repository.hpp"
#include "ores.reporting.core/repository/cut_kind_repository.hpp"
#include "ores.reporting.core/repository/cut_repository.hpp"
#include "ores.reporting.core/service/cut_assembly.hpp"
#include "ores.reporting.core/service/cut_state.hpp"
#include "ores.reporting.core/service/cut_take_plan.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <chrono>
#include <format>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace ores::reporting::service {

using namespace ores::logging;
namespace db = ores::database::repository;
using ores::platform::time::datetime;
using ores::utility::domain::outcome;

namespace {

/** The instant at the precision the store keeps, so two statements of it agree. */
std::chrono::system_clock::time_point at_store_precision(
    const std::chrono::system_clock::time_point& t) {
    return std::chrono::floor<std::chrono::microseconds>(t);
}

/**
 * @brief The instant a take states, or the sentence that refuses it.
 *
 * A time in the future is refused because a body cannot be fixed at an instant
 * that has not happened, and the refusal says so rather than letting the store
 * hold a body of a day nobody has closed.
 */
std::expected<std::chrono::system_clock::time_point, std::string>
instant_of(const std::string& text) {
    if (text.empty())
        return std::unexpected(
            std::string("State the instant the body is fixed at, so the body holds a timepoint."));
    std::chrono::system_clock::time_point at;
    try {
        at = at_store_precision(datetime::from_iso8601_utc(text));
    } catch (const std::exception&) {
        return std::unexpected("The instant is stated as an ISO 8601 UTC timestamp, and '" + text +
                               "' is not one.");
    }
    if (at > std::chrono::system_clock::now())
        return std::unexpected(
            std::string("A body cannot be fixed at an instant that has not happened: ") + text +
            " is in the future.");
    return at;
}

/** A refusal as the caller reads it: the outcome and one sentence. */
ores::utility::domain::result refuse_with(outcome o, std::string message) {
    return {.outcome = o, .code = std::string{}, .message = std::move(message), .fields = {}};
}

/** The party the take fixes a body for, or a refusal when the session names none. */
std::expected<boost::uuids::uuid, std::string> party_of(const ores::database::context& ctx) {
    if (!ctx.party_id() || ctx.party_id()->is_nil())
        return std::unexpected(
            std::string("A cut belongs to a party, and this session stands in none. Choose the "
                        "party the body is fixed for and take the cut again."));
    return *ctx.party_id();
}

}

cut_take_service::cut_take_service(context ctx, cut_body_reader& reader)
    : ctx_(std::move(ctx))
    , reader_(reader) {}

ores::reporting::messaging::take_cut_response cut_take_service::take(
    const ores::reporting::messaging::take_cut_request& request) {
    ores::reporting::messaging::take_cut_response response;

    // The refusals that need no read are answered first, so a typo costs no
    // database work and the person reads what to state instead.
    if (request.kind_code.empty()) {
        response.result = refuse_with(outcome::invalid, "Choose the kind of cut to take.");
        return response;
    }
    if (request.scope_code.empty()) {
        response.result =
            refuse_with(outcome::invalid, "Name the scope the body fixes, so the cut states books.");
        return response;
    }
    if (request.business_centre_code.empty()) {
        response.result = refuse_with(outcome::invalid,
                                     "Name the rates centre the scope revalues against, so the "
                                     "market data half denotes one centre.");
        return response;
    }
    const auto party = party_of(ctx_);
    if (!party) {
        response.result = refuse_with(outcome::invalid, party.error());
        return response;
    }
    const auto instant = instant_of(request.timepoint);
    if (!instant) {
        response.result = refuse_with(outcome::invalid, instant.error());
        return response;
    }

    repository::cut_kind_repository kinds_repo;
    const auto kinds = kinds_repo.read_latest(ctx_, request.kind_code);
    if (kinds.empty()) {
        response.result = refuse_with(
            outcome::missing,
            "No kind of cut is called " + request.kind_code +
                ", so there is no retention and no authorisation basis to take the body under.");
        return response;
    }
    const auto& kind = kinds.front();
    if (kind.family_code == "consumer") {
        response.result = refuse_with(
            outcome::invalid,
            "A " + kind.code +
                " cut is minted by the run that consumes it, so there is no take to press.");
        return response;
    }

    // The assembly reads what the store answers and records what it cannot.
    const auto facts = reader_.read(cut_body_query{
        .party_id = *party,
        .business_centre_code = request.business_centre_code,
        .timepoint = *instant});
    const auto assembly = assemble_cut_body(kind, facts);

    response.parts = assembly.parts;
    // The gaps are the answer the person reads, beside the parts that closed:
    // a body that could not be assembled says which parts and why.
    for (const auto& gap : assembly.gaps) {
        domain::cut_gap row;
        row.party_id = *party;
        row.part_code = gap.part_code;
        row.item = gap.item;
        row.reason = gap.reason;
        response.gaps.push_back(std::move(row));
    }
    response.deal_count = assembly.deal_count;
    response.market_data_set_count = assembly.market_data_set_count;
    response.reference_data_version = facts.reference_data_version.value_or(std::string{});
    response.engine_version = facts.engine_version.value_or(std::string{});
    response.code_version = facts.code_version.value_or(std::string{});
    response.requires_authorisation = kind.approval_kind_code.has_value();
    response.output_is_indicative = kind.output_is_indicative;
    response.retention_months = kind.retention_months;
    response.retention_destination_code = kind.retention_destination_code;
    if (kind.approval_kind_code) {
        response.approval_kind_code = *kind.approval_kind_code;
        ores::inbox::service::approval_lifecycle lifecycle(ctx_);
        const auto approval_kind = lifecycle.kind(*kind.approval_kind_code);
        if (approval_kind) {
            response.approval_kind_name = approval_kind->name;
            response.decide_permission_code = approval_kind->decide_permission_code;
        }
    } else {
        response.approval_kind_name = "none required";
    }

    // A preview answers the same parts and gaps and writes nothing, so the
    // person who reviews the body reads the body the press would fix.
    if (request.preview) {
        response.result = refuse_with(outcome::ok, {});
        return response;
    }

    // A gap stops the press, and a take of a kind that needs a signature needs
    // a reason for the decider to read.
    const auto needs_reason = kind.approval_kind_code.has_value() && assembly.complete();
    if (needs_reason && request.reason.empty()) {
        response.result =
            refuse_with(outcome::invalid,
                       "Say why the body is being fixed: a cut of the kind " + kind.code +
                           " waits on a signature, and the decider reads the reason.");
        return response;
    }

    const auto taken_at = at_store_precision(std::chrono::system_clock::now());
    const auto actor = ctx_.actor().empty() ? ctx_.service_account() : ctx_.actor();

    // The takes of the occasion are read over the whole natural key, because a
    // retake of the same instant is its own body and its own ordinal.
    repository::cut_repository cuts_repo;
    auto same_occasion = cuts_repo.read_by_kind_and_month(
        ctx_, kind.code, *instant, *instant + std::chrono::seconds(1));
    same_occasion.erase(std::remove_if(same_occasion.begin(),
                                       same_occasion.end(),
                                       [&](const auto& cut) {
                                           return cut.party_id != *party ||
                                                  cut.scope_code != request.scope_code ||
                                                  cut.business_centre_code !=
                                                      request.business_centre_code ||
                                                  cut.timepoint != *instant;
                                       }),
                        same_occasion.end());

    cut_take_plan plan;
    plan.kind_code = kind.code;
    plan.scope_code = request.scope_code;
    plan.business_centre_code = request.business_centre_code;
    plan.party_id = *party;
    plan.timepoint = *instant;
    plan.timepoint_timezone = request.timepoint_timezone;
    plan.cut_time_code =
        request.cut_time_code.empty() ? std::nullopt : std::optional{request.cut_time_code};
    plan.deal_population_id = std::nullopt;
    plan.market_data_snapshot_id = std::nullopt;
    plan.reference_data_version = facts.reference_data_version;
    plan.engine_version = facts.engine_version;
    plan.code_version = facts.code_version;
    plan.taken_by = actor;
    plan.taken_at = taken_at;
    plan.change_reason_code = "system.new_record";
    plan.change_commentary = request.reason.empty() ? "Cut taken by hand" : request.reason;

    auto cut = plan_take(plan, kind, boost::uuids::random_generator{}(),
                         next_take_ordinal(same_occasion));
    cut.tenant_id = ctx_.tenant_id();
    cut.modified_by = actor;
    cut.performed_by = ctx_.service_account();

    // A gap keeps the attempt as a draft: the body is not fixed, so no taking
    // instant holds and no request is raised. The retention, the indicative
    // mark and the authorisation basis are still copied, because they are what
    // the draft promises when the gaps are closed.
    if (!assembly.complete())
        cut.taken_at = std::nullopt;

    std::string request_id;
    {
        // The body, its gaps, the request and the cut's detail land together or
        // not at all: a take that wrote the body and then failed to raise the
        // request would leave a cut nobody is asked to sign.
        db::unit_of_work uow(ctx_);
        const auto& ctx = uow.ctx();

        if (assembly.complete() && kind.approval_kind_code) {
            ores::inbox::service::approval_lifecycle lifecycle(ctx);
            const auto approval_kind = lifecycle.kind(*kind.approval_kind_code);
            if (!approval_kind)
                throw std::runtime_error("No such kind of request: " + *kind.approval_kind_code);
            const auto me = lifecycle.actor_account_id();
            if (!me)
                throw std::runtime_error("The signed-in account was not found.");
            const auto raised = lifecycle.raise(*approval_kind, request.reason, *me);
            request_id = boost::uuids::to_string(raised.id);
            cut.approval_request_id = raised.id;
        }

        repository::cut_repository{}.write(ctx, cut);

        std::vector<domain::cut_gap> gaps;
        gaps.reserve(assembly.gaps.size());
        for (const auto& g : assembly.gaps) {
            domain::cut_gap row;
            row.id = boost::uuids::random_generator{}();
            row.tenant_id = ctx.tenant_id();
            row.party_id = *party;
            row.cut_id = cut.id;
            row.part_code = g.part_code;
            row.item = g.item;
            row.reason = g.reason;
            row.modified_by = actor;
            row.performed_by = ctx.service_account();
            row.change_reason_code = "system.new_record";
            row.change_commentary = g.reason;
            gaps.push_back(std::move(row));
        }
        if (!gaps.empty())
            repository::cut_gap_repository{}.write(ctx, gaps);

        if (!request_id.empty()) {
            domain::cut_approval_detail detail;
            detail.id = boost::uuids::random_generator{}();
            detail.tenant_id = ctx.tenant_id();
            detail.request_id = *cut.approval_request_id;
            detail.party_id = *party;
            detail.cut_id = cut.id;
            detail.deal_count = assembly.deal_count;
            detail.market_data_set_count = assembly.market_data_set_count;
            detail.gap_count = static_cast<int>(assembly.gaps.size());
            detail.applied_by = "";
            detail.applied_at = std::nullopt;
            detail.modified_by = actor;
            detail.performed_by = ctx.service_account();
            detail.change_reason_code = "system.new_record";
            detail.change_commentary =
                "The body cut " + boost::uuids::to_string(cut.id) + " fixed";
            repository::cut_approval_detail_repository{}.write(ctx, detail);
        }
        uow.commit();
    }

    // The request stands whether or not anybody is told, so the telling happens
    // after the commit and a failure there is only logged.
    if (!request_id.empty()) {
        try {
            ores::inbox::service::approval_lifecycle lifecycle(ctx_);
            const auto approval_kind = lifecycle.kind(*kind.approval_kind_code);
            const auto raised = lifecycle.request(request_id);
            if (approval_kind && raised)
                ores::inbox::service::approval_announcer(ctx_).tell_open_parts(
                    lifecycle, *approval_kind, *raised);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), warn)
                << "The request " << request_id << " stands, and telling its deciders failed: "
                << e.what();
        }
    }

    response.cut_id = boost::uuids::to_string(cut.id);
    response.cut_label = cut_label_of(cut, kind);
    response.cut_state = std::string(to_string(derive_state(cut, static_cast<int>(assembly.gaps.size()))));
    response.approval_request_id = request_id;
    if (!assembly.gaps.empty()) {
        // The gaps are the answer: the cut exists as an incomplete body and the
        // sentence says what the person chases. It is not a refused call.
        BOOST_LOG_SEV(lg(), info) << "Cut " << response.cut_id << " of kind " << cut.kind_code
                                  << " was saved incomplete with " << assembly.gaps.size()
                                  << " gap(s).";
    } else {
        BOOST_LOG_SEV(lg(), info) << "Took cut " << response.cut_id << " of kind " << cut.kind_code
                                  << " for scope " << cut.scope_code;
    }
    response.result = refuse_with(outcome::ok, {});
    return response;
}

}
