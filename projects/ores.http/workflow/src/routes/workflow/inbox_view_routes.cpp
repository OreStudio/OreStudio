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
#include "ores.http/routes/workflow/inbox_view_routes.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.api/net/view_answer.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http/routes/workflow/page_query.hpp"
#include "ores.http/routes/workflow/request_joins.hpp"
#include "ores.inbox.api/messaging/approval_operations_protocol.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/published_curves_operations_protocol.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/codec/text_form.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <map>
#include <set>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::http::routes::workflow {

namespace {

namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace inbox_messaging = ores::inbox::messaging;
namespace marketdata_messaging = ores::marketdata::messaging;
namespace reporting_messaging = ores::reporting::messaging;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::nats::service::nats_client;

/// The trail the inbox leaves when a subject of a kind is deliberately absent.
auto& inbox_lg() {
    static auto instance = ores::logging::make_logger("ores.http.workflow.inbox_view_routes");
    return instance;
}

/// The most rows one page of the inbox may ask for. A page costs a join per row.
constexpr std::uint32_t inbox_page_max = 100;

/// The page the inbox answers with when the caller states none.
constexpr std::uint32_t inbox_page_default = 50;

/// The sentence a page states when it is not one.
constexpr std::string_view inbox_page_refused = "A page names an offset and a limit of 1 to 100.";

/// How long the inbox stays fresh: short, because a decision empties a row.
constexpr std::chrono::seconds inbox_stale_after{15};

/// The kinds whose subject the market data component keeps.
constexpr std::string_view curve_publish_kind = "marketdata.curve_publish";

/// The kinds whose subject a cut's detail row holds.
constexpr std::string_view eod_cut_kind = "reporting.eod_cut_authorisation";
constexpr std::string_view ipv_cut_kind = "reporting.ipv_cut_authorisation";

/// Whether a kind keeps a subject in the market data component.
bool is_publish_kind(const std::string& kind) {
    return kind == curve_publish_kind;
}

/// Whether a kind keeps a subject in the reporting component.
bool is_cut_kind(const std::string& kind) {
    return kind == eod_cut_kind || kind == ipv_cut_kind;
}

/// The instant as the wire states it.
std::string instant_text(const std::chrono::system_clock::time_point& at) {
    return ores::utility::codec::text_form<std::chrono::system_clock::time_point>::to_text(at);
}

/// What a subject join found, and why one is absent.
struct subject_join final {
    std::map<std::string, std::string> by_request;
    std::string absent_because = "missing";
};

/**
 * The row with the subject its kind keeps, or the reason it carries none.
 *
 * The member is named on the row and not inside the header, because a subject
 * belongs to the kind that keeps it: a role request and a book change carry
 * neither member, so their rows are the rows they were before the join existed.
 */
std::string with_subject(const std::string& row,
                         std::string_view member,
                         std::string_view state_member,
                         const subject_join& found,
                         const std::string& request_id) {
    const auto subject = found.by_request.find(request_id);
    if (subject == found.by_request.end())
        return rr::object_with_members(
            row, {{std::string(state_member), json::write(found.absent_because)}});
    return rr::object_with_members(row, {{std::string(member), subject->second}});
}


/// The curve publish each request of a page authorises.
///
/// A publish request's subject is the market data component's own detail row,
/// keyed by the request's id, and one read answers the whole page. A reader who may
/// not read the curves keeps the page and loses the subject, and the reason travels
/// with the absence: a refusal is a grant to ask for, and a set that is not there is
/// a fact about the request.
subject_join published_subjects_of(const http_request& req,
                                   nats_client& session,
                                   const std::vector<std::string>& request_ids) {
    subject_join out;
    if (request_ids.empty())
        return out;
    try {
        marketdata_messaging::list_published_curves_request msg;
        msg.include_candidates = true;
        msg.include_ended = true;
        msg.request_id_one_of = request_ids;
        msg.limit = static_cast<std::uint32_t>(request_ids.size());
        const auto reply = forwarding::forward(req, session, msg);
        if (!reply.success) {
            BOOST_LOG_SEV(inbox_lg(), ores::logging::debug)
                << "The curve publish subjects of a page were refused: " << reply.message;
            return {std::map<std::string, std::string>{}, "refused"};
        }
        for (const auto& set : reply.sets) {
            if (set.request_id.empty())
                continue;
            // A set and its request are one subject: when two sets name the same
            // request the first stands, which is the newest the service orders.
            out.by_request.emplace(set.request_id, inbox_view_routes::published_subject_json(set));
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(inbox_lg(), ores::logging::warn)
            << "The curve publish subjects of a page were not read: " << e.what();
        return {std::map<std::string, std::string>{}, "refused"};
    }
    return out;
}

/// The cut each request of a page authorises, and why one is absent.
subject_join cut_subjects_of(const http_request& req,
                             nats_client& session,
                             const std::vector<std::string>& request_ids) {
    subject_join out;
    if (request_ids.empty())
        return out;
    try {
        reporting_messaging::list_cut_authorisation_subjects_request msg;
        msg.request_ids = request_ids;
        const auto reply = forwarding::forward(req, session, msg);
        if (reply.result.outcome != ores::utility::domain::outcome::ok) {
            BOOST_LOG_SEV(inbox_lg(), ores::logging::debug)
                << "The cut subjects of a page were refused: " << reply.result.message;
            return {std::map<std::string, std::string>{}, "refused"};
        }
        for (const auto& subject : reply.subjects) {
            if (subject.request_id.empty())
                continue;
            // A cut and its request are one subject, and the first stands.
            out.by_request.emplace(subject.request_id,
                                   inbox_view_routes::cut_subject_json(subject));
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(inbox_lg(), ores::logging::warn)
            << "The cut subjects of a page were not read: " << e.what();
        return {std::map<std::string, std::string>{}, "refused"};
    }
    return out;
}

/**
 * One page of requests, each drawn with its joins.
 *
 * The page draws no parts: where each part of a request stands is read for the
 * one request a person opened, not for a page, so a list answers an empty list
 * of them rather than paying a read per row for what it does not draw.
 */
std::string page_json(const std::vector<ores::inbox::domain::approval_request>& rows,
                      const http_request& req,
                      nats_client& session) {
    std::vector<boost::uuids::uuid> ids;
    ids.reserve(rows.size());
    for (const auto& row : rows)
        ids.push_back(row.id);

    const auto requesters = joins::requesters_of(req, session, ids);
    const auto subjects = joins::subjects_of_page(req, session, ids);
    const auto decisions = joins::decisions_of(req, session, ids);
    std::set<std::string> accounts;
    for (const auto& row : rows) {
        const auto found = requesters.find(rr::as_text(row.id));
        accounts.insert(found == requesters.end() ? rr::as_text(row.requested_by) : found->second);
    }
    for (const auto& [id, decision] : decisions)
        accounts.insert(rr::as_text(decision.decided_by));
    const auto usernames = joins::usernames_of(req, session, accounts);

    std::vector<std::string> publish_ids;
    std::vector<std::string> cut_ids;
    for (const auto& row : rows) {
        if (is_publish_kind(row.kind_code))
            publish_ids.push_back(rr::as_text(row.id));
        else if (is_cut_kind(row.kind_code))
            cut_ids.push_back(rr::as_text(row.id));
    }
    const auto published = published_subjects_of(req, session, publish_ids);
    const auto cuts = cut_subjects_of(req, session, cut_ids);

    std::vector<std::string> written;
    written.reserve(rows.size());
    for (const auto& row : rows) {
        const auto found = subjects.find(rr::as_text(row.id));
        std::vector<std::string> written_subjects;
        if (found != subjects.end()) {
            written_subjects.reserve(found->second.size());
            for (const auto& link : found->second)
                written_subjects.push_back(joins::subject_json(link));
        }
        auto drawn = joins::request_json(row,
                                         joins::roles_of(req, session, rr::as_text(row.id)),
                                         requesters,
                                         usernames,
                                         decisions,
                                         "[]",
                                         rr::array_of(written_subjects));

        // The subject a kind keeps travels beside the header rather than in it: a
        // kind with no such subject carries neither member, so its row is the row
        // it was before the join existed.
        const auto id = rr::as_text(row.id);
        if (is_publish_kind(row.kind_code))
            drawn = with_subject(drawn, "publishedSubject", "publishedSubjectState", published, id);
        else if (is_cut_kind(row.kind_code))
            drawn = with_subject(drawn, "cutSubject", "cutSubjectState", cuts, id);
        written.push_back(std::move(drawn));
    }
    return rr::array_of(written);
}

}

std::string
inbox_view_routes::published_subject_json(const marketdata_messaging::published_curve_set& set) {
    return rr::object_of(
        {"\"publish_id\":" + json::write(set.publish_id),
         "\"curve\":" + json::write(set.curve),
         "\"party\":" + json::write(set.party),
         "\"party_id\":" + json::write(set.party_id),
         "\"series_id\":" + json::write(set.series_id),
         "\"as_of\":" + json::write(instant_text(set.as_of)),
         "\"point_count\":" + std::to_string(set.point_count),
         "\"state_code\":" + json::write(set.state_code),
         "\"published_at\":" + (set.published_at ? json::write(instant_text(*set.published_at)) :
                                                   std::string("null"))});
}

std::string
inbox_view_routes::cut_subject_json(const reporting_messaging::cut_authorisation_subject& subject) {
    return rr::object_of(
        {"\"cut_id\":" + json::write(subject.cut_id),
         "\"label\":" + json::write(subject.label),
         "\"kind_code\":" + json::write(subject.kind_code),
         "\"kind_name\":" + json::write(subject.kind_name),
         "\"timepoint\":" + json::write(subject.timepoint),
         "\"timepoint_timezone\":" + json::write(subject.timepoint_timezone),
         "\"scope_code\":" + json::write(subject.scope_code),
         "\"business_centre_code\":" + json::write(subject.business_centre_code),
         "\"state\":" + json::write(subject.state),
         "\"deal_count\":" + std::to_string(subject.deal_count),
         "\"market_data_set_count\":" + std::to_string(subject.market_data_set_count),
         "\"gap_count\":" + std::to_string(subject.gap_count),
         "\"applied_by\":" + json::write(subject.applied_by),
         "\"applied_at\":" +
             (subject.applied_at.empty() ? std::string("null") : json::write(subject.applied_at))});
}

http_response inbox_view_routes::inbox_answer(const http_request& req, std::string body) {
    return ores::http::net::view_answer(
        req, std::move(body), ores::http::net::view_cache_policy::fresh_for(inbox_stale_after));
}

void inbox_view_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"workflow"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/requests")
            .summary("The requests this person may decide, and those answered and still in view")
            .description("A composed view owned by ores.workflow: the queue is the inbox "
                         "service's own read, which picks what this person may decide from the "
                         "permission each request's kind names and leaves out what they raised "
                         "themselves. Each row carries the roles it asks for, the latest thing "
                         "said about it, and the trades it is about, each a read of its own "
                         "beside the page. The answered tail travels beside the open queue, "
                         "because a notice that links back to a decision opens a request the "
                         "queue no longer holds. The answer carries an ETag and stays fresh for "
                         "15 seconds.")
            .query_param("offset", "integer", "int32", false, "How many requests to skip")
            .query_param("limit", "integer", "int32", false, "How many requests to return")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<inbox_messaging::list_approval_queue_request>([&] {
                    const auto asked =
                        page_of(req, inbox_page_default, inbox_page_max, inbox_page_refused);

                    inbox_messaging::list_approval_queue_request msg;
                    msg.offset = static_cast<int>(asked.offset);
                    msg.limit = static_cast<int>(asked.limit);
                    const auto reply = forwarding::forward(req, session, msg);
                    rr::require_ok(reply.result);

                    return inbox_answer(
                        req,
                        "{\"items\":" + page_json(reply.requests, req, session) +
                            ",\"total\":" + std::to_string(reply.total) +
                            ",\"answered\":" + page_json(reply.answered, req, session) + "}");
                });
            }));
}

}
