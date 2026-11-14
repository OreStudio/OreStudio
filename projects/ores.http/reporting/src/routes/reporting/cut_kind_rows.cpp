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
#include "ores.http/routes/reporting/cut_kind_rows.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.inbox.api/messaging/approval_kind_protocol.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/messaging/cut_family_protocol.hpp"
#include "ores.reporting.api/messaging/cut_resolution_rule_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <sstream>

namespace ores::http::routes::reporting::cuts {

namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace reporting_messaging = ores::reporting::messaging;
namespace inbox_messaging = ores::inbox::messaging;

using ores::http::domain::http_request;
using ores::nats::service::nats_client;

namespace {

/// The most rows of a closed lookup this part reads.
constexpr std::uint32_t lookup_limit = 1000;

auto& kind_lg() {
    static auto instance = ores::logging::make_logger("ores.http.reporting.cut_kind_rows");
    return instance;
}

/// The names of one lookup, keyed by code.
std::map<std::string, std::string> names_of(const http_request& req,
                                            nats_client& session,
                                            reporting_messaging::list_cut_families_request msg) {
    msg.limit = lookup_limit;
    const auto reply = forwarding::forward(req, session, msg);
    std::map<std::string, std::string> names;
    for (const auto& row : reply.families)
        names.emplace(row.code, row.name);
    return names;
}

/// The names of the resolution rules, keyed by code.
std::map<std::string, std::string> rule_names_of(const http_request& req,
                                                 nats_client& session) {
    reporting_messaging::list_cut_resolution_rules_request msg;
    msg.limit = lookup_limit;
    const auto reply = forwarding::forward(req, session, msg);
    std::map<std::string, std::string> names;
    for (const auto& row : reply.rules)
        names.emplace(row.code, row.name);
    return names;
}

/**
 * The inbox's approval kinds, keyed by code.
 *
 * An account that may not read the inbox gets an empty set rather than a failed
 * screen, because a kind's own row states whether a signature is required
 * without naming the decider. Only a refusal is read that way; an outage is
 * raised.
 */
std::map<std::string, approval_kind> approval_kinds_of(const http_request& req,
                                                       nats_client& session) {
    std::map<std::string, approval_kind> kinds;
    try {
        inbox_messaging::list_approval_kinds_request msg;
        msg.limit = lookup_limit;
        const auto reply = forwarding::forward(req, session, msg);
        for (const auto& row : reply.kinds)
            kinds.emplace(row.code,
                          approval_kind{row.code,
                                        row.name,
                                        std::optional<int>(row.approvals_required),
                                        row.decide_permission_code.empty() ?
                                            std::nullopt :
                                            std::optional<std::string>(row.decide_permission_code)});
    } catch (const rr::refusal& refused) {
        if (refused.status != ores::http::domain::http_status::forbidden)
            throw;
        BOOST_LOG_SEV(kind_lg(), ores::logging::debug)
            << "The approval kinds are not readable by this account, so a kind names no decider.";
    }
    return kinds;
}

std::string kind_text(const std::optional<std::string>& value) {
    return value ? json::write(*value) : std::string("null");
}

std::string kind_number(const std::optional<int>& value) {
    return value ? std::to_string(*value) : std::string("null");
}

}

kind_lookups read_kind_lookups(const http_request& req, nats_client& session) {
    reporting_messaging::list_cut_families_request families;
    return kind_lookups{names_of(req, session, families),
                        rule_names_of(req, session),
                        approval_kinds_of(req, session)};
}

kind_row row_of(const ores::reporting::domain::cut_kind& kind,
                const kind_lookups& lookups,
                int cuts_count) {
    const auto family = lookups.families.find(kind.family_code);
    const auto rule = lookups.rules.find(kind.resolution_rule_code);
    const auto approval =
        kind.approval_kind_code ? lookups.approvals.find(*kind.approval_kind_code) :
                                  lookups.approvals.end();

    kind_row row;
    row.code = kind.code;
    row.name = kind.name;
    row.description = kind.description;
    row.family_code = kind.family_code;
    row.family_name =
        family == lookups.families.end() ? kind.family_code : family->second;
    // The signed-or-not a person reads is the approval kind's presence, and
    // nothing else: a firm that gives the close elsewhere changes one approval
    // kind and no kind of cut.
    row.signed_by_kind = kind.approval_kind_code.has_value();
    row.approval_kind_code = kind.approval_kind_code;
    if (kind.approval_kind_code) {
        const auto found = approval;
        row.approval_kind_name =
            found == lookups.approvals.end() ? *kind.approval_kind_code : found->second.name;
        if (found != lookups.approvals.end()) {
            row.approvals_required = found->second.approvals_required;
            row.decide_permission_code = found->second.decide_permission_code;
        }
    }
    row.output_is_indicative = kind.output_is_indicative;
    row.resolution_rule_code = kind.resolution_rule_code;
    row.resolution_rule_name =
        rule == lookups.rules.end() ? kind.resolution_rule_code : rule->second;
    row.retention_months = kind.retention_months;
    row.retention_destination_code = kind.retention_destination_code;
    row.legal_hold_default = kind.legal_hold_default;
    row.display_order = kind.display_order;
    row.cuts_count = cuts_count;
    row.version = kind.version;
    return row;
}

std::optional<kind_row> read_one_kind(const http_request& req,
                                      nats_client& session,
                                      const std::string& code) {
    const auto lookups = read_kind_lookups(req, session);

    // The count of one kind's cuts is that kind's own read, so a record costs
    // one cut read and not the party's history.
    reporting_messaging::list_cut_rows_request ask;
    ask.kind_code = code;
    ask.limit = 1;
    const auto counted = forwarding::forward(req, session, ask);

    reporting_messaging::list_cut_kinds_request msg;
    msg.limit = lookup_limit;
    const auto reply = forwarding::forward(req, session, msg);
    for (const auto& kind : reply.kinds)
        if (kind.code == code)
            return row_of(kind, lookups, counted.total);
    return std::nullopt;
}

kind_row row_of_a_cut(const ores::reporting::domain::cut& cut) {
    kind_row row;
    row.code = cut.kind_code;
    row.signed_by_kind = cut.requires_authorisation;
    row.output_is_indicative = cut.output_is_indicative;
    row.retention_months = cut.retention_months;
    row.retention_destination_code = cut.retention_destination_code;
    return row;
}

std::string cut_row_json(const ores::reporting::messaging::cut_row& row) {
    const auto text_or_null = [](const std::string& stated) {
        return stated.empty() ? std::string("null") : json::write(stated);
    };
    std::ostringstream body;
    body << "{\"id\":" << json::write(row.cut_id) << ",\"label\":" << json::write(row.label)
         << ",\"state\":" << json::write(row.state)
         << ",\"kindCode\":" << json::write(row.kind_code)
         << ",\"kindName\":" << json::write(row.kind_name)
         << ",\"timepoint\":" << json::write(row.timepoint)
         << ",\"timepointTimezone\":" << json::write(row.timepoint_timezone)
         << ",\"scopeCode\":" << json::write(row.scope_code)
         << ",\"businessCentreCode\":" << json::write(row.business_centre_code)
         << ",\"takenBy\":" << json::write(row.taken_by)
         << ",\"takenAt\":" << text_or_null(row.taken_at)
         << ",\"approvedBy\":" << json::write(row.approved_by)
         << ",\"approvedAt\":" << text_or_null(row.approved_at)
         << ",\"refusedAt\":" << text_or_null(row.refused_at)
         << ",\"requiresAuthorisation\":" << (row.requires_authorisation ? "true" : "false")
         << ",\"outputIsIndicative\":" << (row.output_is_indicative ? "true" : "false")
         << ",\"takeOrdinal\":" << row.take_ordinal
         << ",\"runCount\":" << row.run_count
         << ",\"supersededById\":" << text_or_null(row.superseded_by_id) << '}';
    return body.str();
}

std::string kind_json(const kind_row& row) {
    std::ostringstream body;
    body << "{\"code\":" << json::write(row.code) << ",\"name\":" << json::write(row.name)
         << ",\"description\":" << json::write(row.description)
         << ",\"familyCode\":" << json::write(row.family_code)
         << ",\"familyName\":" << json::write(row.family_name)
         << ",\"signed\":" << (row.signed_by_kind ? "true" : "false")
         << ",\"approvalKindCode\":" << kind_text(row.approval_kind_code)
         << ",\"approvalKindName\":" << kind_text(row.approval_kind_name)
         << ",\"approvalsRequired\":" << kind_number(row.approvals_required)
         << ",\"decidePermissionCode\":" << kind_text(row.decide_permission_code)
         << ",\"outputIsIndicative\":" << (row.output_is_indicative ? "true" : "false")
         << ",\"resolutionRuleCode\":" << json::write(row.resolution_rule_code)
         << ",\"resolutionRuleName\":" << json::write(row.resolution_rule_name)
         << ",\"retentionMonths\":" << row.retention_months
         << ",\"retentionDestinationCode\":" << json::write(row.retention_destination_code)
         << ",\"legalHoldDefault\":" << (row.legal_hold_default ? "true" : "false")
         << ",\"displayOrder\":" << row.display_order
         << ",\"cutsCount\":" << row.cuts_count << ",\"version\":" << row.version << '}';
    return body.str();
}

}
