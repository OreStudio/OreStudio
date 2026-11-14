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
#include "ores.http/routes/reporting/cut_routes.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http/routes/reporting/cut_kind_rows.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/messaging/report_instance_protocol.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <algorithm>
#include <boost/uuid/uuid_io.hpp>
#include <map>
#include <optional>
#include <sstream>
#include <string>
#include <vector>

namespace ores::http::routes::reporting {

namespace {

namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace reporting_messaging = ores::reporting::messaging;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;
using ores::utility::codec::field;

auto& cuts_lg() {
    static auto instance = ores::logging::make_logger("ores.http.reporting.cut_routes");
    return instance;
}

/// How many runs of one cut the Runs tab reads, newest start first.
constexpr std::uint32_t runs_of_a_cut_page = 200;

namespace cuts_unit {

/// The seven states a cut may be in, as the service words them.
constexpr std::string_view state_words[] = {"draft",     "incomplete", "taken", "in_use",
                                            "authorised", "refused",   "superseded"};

bool is_state_word(const std::string& stated) {
    return stated.empty() ||
           std::ranges::any_of(state_words, [&](std::string_view word) { return word == stated; });
}

/// The narrowing the cuts list takes, and its page.
struct cut_query final {
    std::string kind_code;
    std::string month;
    std::string search;
    std::string state;
    std::uint32_t offset = 0;
    std::uint32_t limit = 0;
};

cut_query query_of(const http_request& req) {
    rr::refuse_unknown_params(
        req, {"kind_code", "month", "search", "state", "offset", "limit", "sort", "descending"});
    cut_query out;
    out.kind_code = req.get_query_param("kind_code");
    out.month = req.get_query_param("month");
    out.search = rr::trimmed(req.get_query_param("search"));
    out.state = req.get_query_param("state");
    if (out.kind_code.size() > 100 || out.month.size() > 7 || out.search.size() > 256 ||
        !is_state_word(out.state))
        rr::invalid("The cuts list names a kind, a month of the form YYYY-MM, a search of at "
                    "most 256 characters and a state.");
    if (!req.get_query_param("sort").empty())
        rr::invalid("The cuts are read newest instant first and cannot be ordered by " +
                    req.get_query_param("sort") + ".");

    if (const auto raw = req.get_query_param("offset"); !raw.empty()) {
        try {
            out.offset = static_cast<std::uint32_t>(std::stoul(raw));
        } catch (const std::exception&) {
            rr::invalid("A page names an offset and a limit of 1 to 1000.");
        }
    }
    const auto raw_limit = req.get_query_param("limit");
    std::uint32_t stated = 0;
    if (!raw_limit.empty()) {
        try {
            stated = static_cast<std::uint32_t>(std::stoul(raw_limit));
        } catch (const std::exception&) {
            rr::invalid("A page names an offset and a limit of 1 to 1000.");
        }
    }
    if (stated < 1 || stated > 1000)
        rr::invalid("A page names an offset and a limit of 1 to 1000.");
    out.limit = stated;
    return out;
}

/// The gaps the assembly could not fill, as the screen reads them.
std::string gaps_json(const std::vector<ores::reporting::domain::cut_gap>& gaps) {
    std::vector<std::string> written;
    written.reserve(gaps.size());
    for (const auto& gap : gaps) {
        written.push_back("{\"id\":" + json::write(boost::uuids::to_string(gap.id)) +
                          ",\"partCode\":" + json::write(gap.part_code) +
                          ",\"item\":" + json::write(gap.item) +
                          ",\"reason\":" + json::write(gap.reason) + "}");
    }
    return rr::array_of(written);
}

/// The subject of a cut's approval request, or null where the kind required none.
std::string detail_json(const std::optional<ores::reporting::domain::cut_approval_detail>& detail) {
    if (!detail)
        return "null";
    const auto applied_at = detail->applied_at ?
                                json::write(*detail->applied_at) :
                                std::string("null");
    return "{\"id\":" + json::write(boost::uuids::to_string(detail->id)) +
           ",\"requestId\":" + json::write(boost::uuids::to_string(detail->request_id)) +
           ",\"cutId\":" + json::write(boost::uuids::to_string(detail->cut_id)) +
           ",\"dealCount\":" + std::to_string(detail->deal_count) +
           ",\"marketDataSetCount\":" + std::to_string(detail->market_data_set_count) +
           ",\"gapCount\":" + std::to_string(detail->gap_count) +
           ",\"appliedBy\":" + json::write(detail->applied_by) +
           ",\"appliedAt\":" + applied_at + "}";
}

/// One cut's body, as its record page draws it.
std::string body_json(const ores::reporting::domain::cut& cut) {
    const auto text_or_null = [](const std::optional<std::string>& stated) {
        return stated ? json::write(*stated) : std::string("null");
    };
    const auto id_or_null = [](const std::optional<boost::uuids::uuid>& stated) {
        return stated && !stated->is_nil() ? json::write(boost::uuids::to_string(*stated)) :
                                             std::string("null");
    };
    const auto instant_or_null = [](const std::optional<std::chrono::system_clock::time_point>& at) {
        return at ? json::write(*at) : std::string("null");
    };

    std::ostringstream body;
    body << "{\"id\":" << json::write(boost::uuids::to_string(cut.id))
         << ",\"version\":" << cut.version
         << ",\"partyId\":" << json::write(boost::uuids::to_string(cut.party_id))
         << ",\"kindCode\":" << json::write(cut.kind_code)
         << ",\"scopeCode\":" << json::write(cut.scope_code)
         << ",\"businessCentreCode\":" << json::write(cut.business_centre_code)
         << ",\"timepoint\":" << json::write(cut.timepoint)
         << ",\"timepointTimezone\":" << json::write(cut.timepoint_timezone)
         << ",\"takeOrdinal\":" << cut.take_ordinal
         << ",\"cutTimeCode\":" << text_or_null(cut.cut_time_code)
         << ",\"marketDataSnapshotId\":" << id_or_null(cut.market_data_snapshot_id)
         << ",\"dealPopulationId\":" << id_or_null(cut.deal_population_id)
         << ",\"referenceDataVersion\":" << text_or_null(cut.reference_data_version)
         << ",\"engineVersion\":" << text_or_null(cut.engine_version)
         << ",\"codeVersion\":" << text_or_null(cut.code_version)
         << ",\"takenBy\":" << json::write(cut.taken_by)
         << ",\"takenAt\":" << instant_or_null(cut.taken_at)
         << ",\"requiresAuthorisation\":" << (cut.requires_authorisation ? "true" : "false")
         << ",\"approvalRequestId\":" << id_or_null(cut.approval_request_id)
         << ",\"approvedBy\":" << json::write(cut.approved_by)
         << ",\"approvedAt\":" << instant_or_null(cut.approved_at)
         << ",\"refusedAt\":" << instant_or_null(cut.refused_at)
         << ",\"refusalComment\":" << json::write(cut.refusal_comment)
         << ",\"retentionMonths\":" << cut.retention_months
         << ",\"retentionDestinationCode\":" << json::write(cut.retention_destination_code)
         << ",\"legalHold\":" << (cut.legal_hold ? "true" : "false")
         << ",\"outputIsIndicative\":" << (cut.output_is_indicative ? "true" : "false")
         << ",\"modifiedBy\":" << json::write(cut.modified_by)
         << ",\"performedBy\":" << json::write(cut.performed_by)
         << ",\"changeReasonCode\":" << json::write(cut.change_reason_code)
         << ",\"changeCommentary\":" << json::write(cut.change_commentary)
         << ",\"recordedAt\":" << json::write(cut.recorded_at) << '}';
    return body.str();
}

/// The runs that priced from one cut, newest start first.
std::string runs_of_cut(const http_request& req, nats_client& session, const std::string& cut_id) {
    reporting_messaging::list_report_instances_request msg;
    msg.limit = runs_of_a_cut_page;
    msg.filter.emplace();
    msg.filter->cut_id_one_of = std::vector{boost::uuids::string_generator()(cut_id)};
    const auto reply = forwarding::forward(req, session, msg);

    auto runs = reply.instances;
    std::ranges::sort(runs, [](const auto& left, const auto& right) {
        return left.started_at > right.started_at;
    });

    // A failed read of the states leaves the column empty and does not fail the
    // page: the run is the fact and its state is the decoration.
    std::map<std::string, std::pair<std::string, std::string>> state_of;
    std::vector<boost::uuids::uuid> ids;
    for (const auto& run : runs)
        ids.push_back(run.id);
    if (!ids.empty()) {
        try {
            reporting_messaging::get_report_instance_health_request health;
            health.instance_ids = ids;
            const auto answered = forwarding::forward(req, session, health);
            for (const auto& entry : answered.health)
                state_of.emplace(boost::uuids::to_string(entry.report_instance_id),
                                 std::pair{entry.state_name, entry.health});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(cuts_lg(), ores::logging::warn)
                << "The states of a cut's runs could not be read: " << e.what();
        }
    }

    std::vector<std::string> written;
    written.reserve(runs.size());
    for (const auto& run : runs) {
        const auto found = state_of.find(boost::uuids::to_string(run.id));
        const auto instant = [](const auto& at) {
            return at ? json::write(*at) : std::string("null");
        };
        const auto health = found == state_of.end() || found->second.second.empty() ?
                                std::string("null") :
                                json::write(found->second.second);
        written.push_back(
            "{\"id\":" + json::write(boost::uuids::to_string(run.id)) +
            ",\"name\":" + json::write(run.name) +
            ",\"stateName\":" +
            json::write(found == state_of.end() ? std::string() : found->second.first) +
            ",\"health\":" + health + ",\"startedAt\":" + instant(run.started_at) +
            ",\"completedAt\":" + instant(run.completed_at) +
            ",\"pricedAt\":" + instant(run.priced_at) + "}");
    }
    return "{\"runs\":" + rr::array_of(written) + "}";
}

/// The take's own body, as the floor's screen sends it.
struct take_body final {
    std::string kind_code;
    std::string scope_code;
    std::string business_centre_code;
    std::string timepoint;
    std::string timepoint_timezone;
    std::string cut_time_code;
    std::string reason;
    bool preview = false;
};

constexpr auto wire_fields(std::type_identity<take_body>) {
    return ores::utility::codec::field_list{field{"kindCode", &take_body::kind_code},
                                            field{"scopeCode", &take_body::scope_code},
                                            field{"businessCentreCode",
                                                  &take_body::business_centre_code},
                                            field{"timepoint", &take_body::timepoint},
                                            field{"timepointTimezone",
                                                  &take_body::timepoint_timezone},
                                            field{"cutTimeCode", &take_body::cut_time_code},
                                            field{"reason", &take_body::reason},
                                            field{"preview", &take_body::preview}};
}

/// One part of a body the assembly answered, with the state it reached.
std::string part_json(const reporting_messaging::cut_body_part& part) {
    return "{\"partCode\":" + json::write(part.part_code) +
           ",\"state\":" + json::write(std::string(part.state == "gapped" ? "gapped" : "closed")) +
           ",\"summary\":" + json::write(part.summary) +
           ",\"reference\":" + json::write(part.reference) +
           ",\"item\":" + json::write(part.item) +
           ",\"reason\":" + json::write(part.reason) +
           ",\"remedy\":" + json::write(part.remedy) +
           ",\"snapshotNote\":" + json::write(part.snapshot_note) + "}";
}

/// The body one assembly answered, and the take it wrote.
std::string take_json(const reporting_messaging::take_cut_response& reply) {
    std::vector<std::string> parts;
    for (const auto& part : reply.parts)
        parts.push_back(part_json(part));

    std::ostringstream body;
    body << "{\"cutId\":" << json::write(reply.cut_id)
         << ",\"cutLabel\":" << json::write(reply.cut_label)
         << ",\"cutState\":" << json::write(reply.cut_state)
         << ",\"requiresAuthorisation\":" << (reply.requires_authorisation ? "true" : "false")
         << ",\"outputIsIndicative\":" << (reply.output_is_indicative ? "true" : "false")
         << ",\"retentionMonths\":" << reply.retention_months
         << ",\"retentionDestinationCode\":" << json::write(reply.retention_destination_code)
         << ",\"approvalRequestId\":" << json::write(reply.approval_request_id)
         << ",\"approvalKindCode\":" << json::write(reply.approval_kind_code)
         << ",\"approvalKindName\":" << json::write(reply.approval_kind_name)
         << ",\"decidePermissionCode\":" << json::write(reply.decide_permission_code)
         << ",\"parts\":" << rr::array_of(parts)
         << ",\"gaps\":" << gaps_json(reply.gaps)
         << ",\"dealCount\":" << reply.deal_count
         << ",\"marketDataSetCount\":" << reply.market_data_set_count
         << ",\"referenceDataVersion\":" << json::write(reply.reference_data_version)
         << ",\"engineVersion\":" << json::write(reply.engine_version)
         << ",\"codeVersion\":" << json::write(reply.code_version) << '}';
    return body.str();
}

}

}

using namespace cuts_unit;

void cut_routes::register_routes(std::shared_ptr<ores::http::net::router> router,
                                 std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                 nats_client& session) {
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"reporting"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/reporting/cuts")
            .summary("The cuts the party holds, newest instant first")
            .description("Each row beside the state and the label the reporting service derives, "
                         "the acts that made it and the runs that priced from it.")
            .query_param("kind_code", "string", "", false, "Only cuts of this kind")
            .query_param("month", "string", "", false, "A month of the form YYYY-MM")
            .query_param("search", "string", "", false, "A text the label holds")
            .query_param("state", "string", "", false, "One of the seven states")
            .query_param("offset", "integer", "int32", false, "How many cuts to skip")
            .query_param("limit", "integer", "int32", false, "How many cuts to return")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<reporting_messaging::list_cut_rows_request>([&] {
                    const auto asked = query_of(req);
                    reporting_messaging::list_cut_rows_request msg;
                    msg.kind_code = asked.kind_code;
                    msg.month = asked.month;
                    msg.search = asked.search;
                    msg.state = asked.state;
                    msg.offset = asked.offset;
                    msg.limit = asked.limit;
                    const auto reply = forwarding::forward(req, session, msg);

                    std::vector<std::string> written;
                    written.reserve(reply.rows.size());
                    for (const auto& row : reply.rows)
                        written.push_back(cuts::cut_row_json(row));
                    const auto partial =
                        reply.total > static_cast<int>(asked.offset + reply.rows.size()) ?
                            "true" :
                            "false";
                    return http_response::json("{\"rows\":" + rr::array_of(written) +
                                               ",\"total\":" + std::to_string(reply.total) +
                                               ",\"partial\":" + partial + "}");
                });
            }));

    add(router->get("/api/reporting/cuts/{id}/runs")
            .summary("The runs that priced from one cut, newest start first")
            .description("A run records the cut it resolved its report's selection to, so the "
                         "tab reads the runs by that column and their states beside them.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<reporting_messaging::list_report_instances_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    const auto raw = req.get_path_param("id");
                    if (!rr::is_uuid(raw))
                        rr::invalid("The address names an id that is not a uuid.");
                    return http_response::json(runs_of_cut(req, session, raw));
                });
            }));

    add(router->get("/api/reporting/cuts/{id}")
            .summary("One cut's record: its body, the acts that made it and the gaps it stands on")
            .description("The body, the state and label the service derives, the kind it was "
                         "taken under, the gaps the assembly could not fill and the runs that "
                         "priced from it.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<reporting_messaging::get_cut_record_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    const auto raw = req.get_path_param("id");
                    if (!rr::is_uuid(raw))
                        rr::invalid("The address names an id that is not a uuid.");

                    reporting_messaging::get_cut_record_request msg;
                    msg.cut_id = boost::uuids::string_generator()(raw);
                    const auto reply = forwarding::forward(req, session, msg);
                    rr::require_ok(reply.result);
                    if (reply.cut.id.is_nil())
                        rr::refuse(http_status::not_found,
                                   "not-found",
                                   "There is no such cut, or you may not see it.");

                    // The kind the cut names, read once, and the body that holds
                    // its place. No other kind and no other cut is read.
                    auto kind = reply.kind.code.empty() ?
                                    std::optional<cuts::kind_row>() :
                                    cuts::read_one_kind(req, session, reply.kind.code);
                    if (!kind)
                        kind = cuts::row_of_a_cut(reply.cut);

                    std::string superseded_by_label = "null";
                    if (!reply.superseded_by_id.empty()) {
                        try {
                            reporting_messaging::get_cut_record_request later;
                            later.cut_id =
                                boost::uuids::string_generator()(reply.superseded_by_id);
                            const auto answered = forwarding::forward(req, session, later);
                            if (answered.result.outcome == reply.result.outcome &&
                                !answered.label.empty())
                                superseded_by_label = json::write(answered.label);
                        } catch (const std::exception& e) {
                            BOOST_LOG_SEV(cuts_lg(), ores::logging::warn)
                                << "The label of the cut that holds this one's place could not be "
                                   "read: "
                                << e.what();
                        }
                    }

                    return http_response::json(
                        "{\"cut\":" + body_json(reply.cut) +
                        ",\"label\":" + json::write(reply.label) +
                        ",\"state\":" + json::write(reply.state) +
                        ",\"supersededById\":" +
                        (reply.superseded_by_id.empty() ? std::string("null") :
                                                          json::write(reply.superseded_by_id)) +
                        ",\"supersededByLabel\":" + superseded_by_label +
                        ",\"kind\":" + cuts::kind_json(*kind) +
                        ",\"gaps\":" + gaps_json(reply.gaps) +
                        ",\"runCount\":" + std::to_string(reply.run_count) +
                        ",\"detail\":" + detail_json(reply.detail) + "}");
                });
            }));

    add(router->post("/api/reporting/cuts/take")
            .summary("Assemble a body part by part, and press to take it")
            .description("One route serves the floor's assembly and its press, because an "
                         "assembly computed by a second path is a preview of that path. A gap is "
                         "the answer and not a failure.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<reporting_messaging::take_cut_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    const auto body = rr::body_of<take_body>(req,
                                                             {"kindCode",
                                                              "scopeCode",
                                                              "businessCentreCode",
                                                              "timepoint",
                                                              "timepointTimezone",
                                                              "cutTimeCode",
                                                              "reason",
                                                              "preview"},
                                                             "A take names the kind, the scope, "
                                                             "the centre and the instant, and "
                                                             "whether it presses.");
                    if (rr::trimmed(body.kind_code).empty() || body.kind_code.size() > 100)
                        rr::invalid("A take names a kind of cut.");
                    if (rr::trimmed(body.scope_code).empty() || body.scope_code.size() > 100)
                        rr::invalid("A take names a scope.");
                    if (rr::trimmed(body.business_centre_code).empty() ||
                        body.business_centre_code.size() > 100)
                        rr::invalid("A take names a business centre.");
                    if (rr::trimmed(body.timepoint).empty() || body.timepoint.size() > 64)
                        rr::invalid("A take names the instant it fixes.");
                    if (rr::trimmed(body.timepoint_timezone).empty() ||
                        body.timepoint_timezone.size() > 64)
                        rr::invalid("A take names the clock its instant is stated in.");
                    if (body.cut_time_code.size() > 100 || body.reason.size() > 2000)
                        rr::invalid("A take names a cut time of at most 100 characters and a "
                                    "reason of at most 2000.");

                    reporting_messaging::take_cut_request msg;
                    msg.kind_code = body.kind_code;
                    msg.scope_code = body.scope_code;
                    msg.business_centre_code = body.business_centre_code;
                    msg.timepoint = body.timepoint;
                    msg.timepoint_timezone = body.timepoint_timezone;
                    msg.cut_time_code = body.cut_time_code;
                    msg.reason = body.reason;
                    msg.preview = body.preview;
                    const auto reply = forwarding::forward(req, session, msg);
                    rr::require_done(reply.result);
                    return http_response::json(take_json(reply));
                });
            }));
}

}
