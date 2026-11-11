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
#include "ores.http/routes/iam/tenant_roster_routes.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http/routes/iam/deployment_access.hpp"
#include "ores.http/routes/iam/tenancy_screen_shapes.hpp"
#include "ores.iam.api/messaging/tenant_protocol.hpp"
#include "ores.iam.api/messaging/tenant_type_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "ores.workflow.api/messaging/workflow_protocol.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <charconv>
#include <cstddef>
#include <exception>
#include <functional>
#include <limits>
#include <set>
#include <stdexcept>
#include <tuple>
#include <type_traits>

namespace ores::http::routes::iam {

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::iam::domain::tenant;
using ores::nats::service::nats_client;
namespace json = ores::utility::codec::json;
namespace messaging = ores::iam::messaging;
namespace workflow = ores::workflow::messaging;
namespace rr = ores::http::net::record_routes;

namespace {

/// How many tenants and runs the system administrator's home shows.
constexpr std::uint32_t overview_tenants = 5;
constexpr int overview_activity = 5;
constexpr int overview_attention = 10;

/// The most runs one read answers: the protocol's bound on one reply.
constexpr int tenant_setup_read_limit = 1000;

/// The run type and target kind ores.iam names on a tenant's provisioning run.
constexpr std::string_view provision_tenant_workflow_type = "provision_tenant_workflow";
constexpr std::string_view provision_tenant_target_kind = "tenant";

/// The step states that count as finished, as the run rail states them.
constexpr std::string_view finished_step_statuses[]{"completed", "completed_with_warnings"};

std::optional<std::uint32_t> roster_bound(const http_request& req,
                                          const std::string& name,
                                          std::uint32_t fallback,
                                          std::uint32_t least,
                                          std::uint32_t most) {
    const auto text = req.get_query_param(name);
    if (text.empty())
        return fallback;
    std::uint32_t value = 0;
    const auto [end, failed] = std::from_chars(text.data(), text.data() + text.size(), value);
    if (failed != std::errc{} || end != text.data() + text.size() || value < least || value > most)
        return std::nullopt;
    return value;
}

/// What one tenant list read narrows by; a member left empty sets no condition.
struct tenant_list_query final {
    std::string search;
    std::string type;
    std::string status;
    std::optional<std::vector<std::string>> types;
    std::optional<std::vector<boost::uuids::uuid>> ids;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    std::string sort;
    bool descending = false;
};

/// A failed read the BFF stated as an internal failure.
[[noreturn]] void unread(const std::string& message, std::string_view fallback) {
    throw std::runtime_error(message.empty() ? std::string(fallback) : message);
}

messaging::list_tenants_response
tenants_page(const http_request& req, nats_client& session, const tenant_list_query& q) {
    const auto or_none = [](const std::string& text) {
        return text.empty() ? std::nullopt : std::optional<std::string>(text);
    };
    messaging::list_tenants_request msg;
    msg.offset = q.offset;
    msg.limit = q.limit;
    // The tenant repository orders only by the fields it lists as sortable and
    // refuses any other, so the browser's sort reaches no query text.
    msg.order.field = q.sort.empty() ? "code" : q.sort;
    msg.order.descending = q.descending;
    msg.filter = messaging::tenants_filter{
        or_none(q.type), or_none(q.status), q.ids, q.types, std::nullopt, or_none(q.search)};
    auto reply = ores::http::forwarding::forward(req, session, msg);
    if (reply.result.outcome != ores::utility::domain::outcome::ok)
        unread(reply.result.message, "The tenants were not read.");
    return reply;
}

std::vector<std::string> tenant_type_codes(const http_request& req, nats_client& session) {
    const auto listed =
        ores::http::forwarding::forward(req, session, messaging::list_tenant_types_request{});
    std::vector<std::string> codes;
    for (const auto& row : listed.types)
        codes.push_back(row.type);
    return codes;
}

/// How many of a run's steps are finished. A read that fails counts none, so
/// one run whose progress is unavailable does not fail the page that names it.
int run_steps_done(const http_request& req, nats_client& session, const std::string& instance_id) {
    try {
        const auto progress = ores::http::forwarding::forward(
            req, session, workflow::get_workflow_steps_request{instance_id});
        return static_cast<int>(std::ranges::count_if(progress.steps, [](const auto& step) {
            return std::ranges::find(finished_step_statuses, step.status) !=
                   std::end(finished_step_statuses);
        }));
    } catch (const rr::refusal&) {
        return 0;
    } catch (const std::exception&) {
        return 0;
    }
}

workflow::list_workflow_instance_summaries_response
provisioning_runs(const http_request& req,
                  nats_client& session,
                  workflow::list_workflow_instance_summaries_request msg) {
    msg.type_filter = std::string(provision_tenant_workflow_type);
    msg.target_kind_filter = std::string(provision_tenant_target_kind);
    auto reply = ores::http::forwarding::forward(req, session, msg);
    if (!reply.success)
        unread(reply.message, "The provisioning runs were not read.");
    return reply;
}

/// The runs read for the named tenants, and whether the read saw all of them.
struct tenant_setups final {
    std::map<std::string, tenant_roster_routes::tenant_setup> setups;
    bool complete = true;
};

/// The latest provisioning run of each named tenant. The engine answers the run
/// that changed last first, so the first run seen for a tenant is reported.
tenant_setups setups_of(const http_request& req,
                        nats_client& session,
                        const std::vector<std::string>& tenant_ids) {
    tenant_setups out;
    if (tenant_ids.empty())
        return out;
    workflow::list_workflow_instance_summaries_request msg;
    msg.limit = tenant_setup_read_limit;
    msg.target_ids_filter = tenant_ids;
    const auto reply = provisioning_runs(req, session, msg);
    for (const auto& run : reply.instances) {
        if (run.target_id.empty() || out.setups.contains(run.target_id))
            continue;
        out.setups[run.target_id] = {
            run.id, run.status, run_steps_done(req, session, run.id), run.step_count, run.error};
    }
    out.complete = static_cast<int>(reply.instances.size()) < tenant_setup_read_limit;
    return out;
}

/// The tenant a run acts on, or nothing when the run names none or names it
/// malformed: one bad run must not hide the rest of the activity.
std::optional<boost::uuids::uuid> run_target_of(const std::string& target_id) {
    if (target_id.empty())
        return std::nullopt;
    try {
        return boost::uuids::string_generator()(target_id);
    } catch (const std::exception&) {
        return std::nullopt;
    }
}

std::vector<std::string> ids_of(const std::vector<tenant>& tenants) {
    std::vector<std::string> ids;
    for (const auto& t : tenants)
        ids.push_back(boost::uuids::to_string(t.id));
    return ids;
}

/// One tenant by its code, or nothing when no tenant holds the code.
std::optional<tenant>
tenant_by_code(const http_request& req, nats_client& session, const std::string& code) {
    const auto reply = ores::http::forwarding::forward(
        req, session, messaging::get_tenant_request{messaging::tenant_key{code}});
    if (reply.result.outcome == ores::utility::domain::outcome::missing)
        return std::nullopt;
    if (reply.result.outcome != ores::utility::domain::outcome::ok || !reply.tenant)
        unread(reply.result.message, "The tenant was not read.");
    return reply.tenant;
}

std::string setup_json(const std::optional<tenant_roster_routes::tenant_setup>& setup) {
    if (!setup)
        return "null";
    return "{\"instanceId\":" + json::write(setup->instance_id) +
           ",\"status\":" + json::write(setup->status) +
           ",\"stepsDone\":" + std::to_string(setup->steps_done) +
           ",\"stepCount\":" + std::to_string(setup->step_count) +
           ",\"error\":" + json::write(setup->error) + "}";
}

std::string summary_fields(const tenant& t) {
    return "\"id\":" + json::write(boost::uuids::to_string(t.id)) +
           ",\"code\":" + json::write(t.code) + ",\"name\":" + json::write(t.name) +
           ",\"type\":" + json::write(t.type) + ",\"description\":" + json::write(t.description) +
           ",\"hostname\":" + json::write(t.hostname) + ",\"status\":" + json::write(t.status) +
           ",\"registrationDefault\":" + (t.is_registration_default ? "true" : "false");
}

std::optional<tenant_roster_routes::tenant_setup>
setup_for(const std::map<std::string, tenant_roster_routes::tenant_setup>& setups,
          const tenant& t) {
    const auto found = setups.find(boost::uuids::to_string(t.id));
    return found == setups.end() ? std::nullopt : std::optional(found->second);
}

}

tenant tenant_roster_routes::known_tenant(const http_request& req,
                                          nats_client& session,
                                          const std::string& code) {
    auto found = tenant_by_code(req, session, code);
    if (!found)
        throw rr::refusal{http_status::not_found, "not-found", "No tenant has this code."};
    return *found;
}

std::optional<tenant_roster_routes::roster_query>
tenant_roster_routes::read_roster_query(const http_request& req) {
    const auto offset =
        roster_bound(req, "offset", 0, 0, std::numeric_limits<std::uint32_t>::max());
    const auto limit = roster_bound(req, "limit", 100, 1, 1000);
    roster_query q;
    q.search = req.get_query_param("search");
    q.type = req.get_query_param("type");
    q.status = req.get_query_param("status");
    q.include_test = req.get_query_param("includeTest") == "true";
    q.sort = req.get_query_param("sort");
    q.descending = req.get_query_param("descending") == "true";
    if (!offset || !limit || rr::characters_of(q.search) > 200 || rr::characters_of(q.type) > 100 ||
        rr::characters_of(q.status) > 100 || rr::characters_of(q.sort) > 100)
        return std::nullopt;
    q.offset = *offset;
    q.limit = *limit;
    return q;
}

std::string tenant_roster_routes::summary_json(const tenant& t,
                                               const std::optional<tenant_setup>& setup) {
    return "{" + summary_fields(t) + ",\"setup\":" + setup_json(setup) + "}";
}

std::string tenant_roster_routes::page_json(const std::vector<tenant>& tenants,
                                            const std::map<std::string, tenant_setup>& setups,
                                            std::uint64_t total,
                                            bool setup_unavailable,
                                            std::uint64_t hidden_test_count) {
    std::string out = "{\"tenants\":[";
    for (std::size_t i = 0; i < tenants.size(); ++i)
        out += (i == 0 ? "" : ",") + summary_json(tenants[i], setup_for(setups, tenants[i]));
    return out + "],\"totalCount\":" + std::to_string(total) +
           ",\"setupUnavailable\":" + json::write(setup_unavailable) +
           ",\"hiddenTestCount\":" + std::to_string(hidden_test_count) + "}";
}

std::string tenant_roster_routes::detail_json(const tenant& t,
                                              const std::optional<tenant_setup>& setup,
                                              bool setup_unavailable) {
    return "{\"tenant\":{" + summary_fields(t) + ",\"setup\":" + setup_json(setup) +
           ",\"version\":" + std::to_string(t.version) +
           ",\"modifiedBy\":" + json::write(t.modified_by) +
           ",\"performedBy\":" + json::write(t.performed_by) +
           ",\"changeReasonCode\":" + json::write(t.change_reason_code) +
           ",\"changeCommentary\":" + json::write(t.change_commentary) +
           ",\"recordedAt\":" + json::write(t.recorded_at) +
           "},\"setupUnavailable\":" + json::write(setup_unavailable) + "}";
}

std::string tenant_roster_routes::overview_json(const overview& o) {
    std::string out = "{\"inService\":" + std::to_string(o.in_service) +
                      ",\"onEvaluation\":" + std::to_string(o.on_evaluation) +
                      ",\"settingUp\":" + std::to_string(o.setting_up) + ",\"attention\":[";
    for (std::size_t i = 0; i < o.attention.size(); ++i) {
        const auto& a = o.attention[i];
        out += (i == 0 ? "{" : ",{");
        out += "\"tenant\":" + summary_json(a.tenant, a.setup) +
               ",\"reason\":" + json::write(a.reason) + "}";
    }
    out += "],\"tenants\":[";
    for (std::size_t i = 0; i < o.tenants.size(); ++i)
        out += (i == 0 ? "" : ",") + summary_json(o.tenants[i], setup_for(o.setups, o.tenants[i]));
    out += "],\"totalCount\":" + std::to_string(o.total_count) + ",\"activity\":[";
    for (std::size_t i = 0; i < o.activity.size(); ++i) {
        const auto& a = o.activity[i];
        out += (i == 0 ? "{" : ",{");
        out += "\"instanceId\":" + json::write(a.instance_id);
        out += ",\"tenantName\":" + json::write(a.tenant_name);
        out += ",\"status\":" + json::write(a.status);
        out += ",\"stepsDone\":" + std::to_string(a.steps_done);
        out += ",\"stepCount\":" + std::to_string(a.step_count);
        out += ",\"error\":" + json::write(a.error);
        out += ",\"at\":" + json::write(a.at) + "}";
    }
    return out + "],\"activityUnavailable\":" + json::write(o.activity_unavailable) + "}";
}

void tenant_roster_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    const auto add = [&](ores::http::net::route_builder builder,
                         std::string summary,
                         std::function<http_response(const http_request&)> handle) {
        const auto built =
            builder.summary(std::move(summary))
                .tags({"tenancy"})
                .auth_required()
                .handler([handle = std::move(handle)](const http_request& req)
                             -> boost::asio::awaitable<http_response> { co_return handle(req); })
                .build();
        router->add_route(built);
        registry->register_route(built);
    };
    using ores::http::forwarding::guarded;
    using namespace ores::logging;

    add(router->get("/api/tenants"),
        "Read a page of the deployment's tenants",
        [&session](const http_request& req) {
            return guarded<messaging::list_tenants_request>([&] {
                deployment_access::require_read(req, session, "The tenants of a deployment");
                const auto q = read_roster_query(req);
                if (!q)
                    return rr::refused(http_status::bad_request,
                                       "invalid-request",
                                       "The page offset and limit must be whole numbers.");
                const auto hide_test = !q->include_test && q->type != test_tenant_type;
                auto shown = tenant_type_codes(req, session);
                if (hide_test)
                    std::erase(shown, std::string(test_tenant_type));
                const auto page = tenants_page(req,
                                               session,
                                               {q->search,
                                                q->type,
                                                q->status,
                                                shown,
                                                std::nullopt,
                                                q->offset,
                                                q->limit,
                                                q->sort,
                                                q->descending});
                std::uint64_t hidden = 0;
                if (hide_test && q->type.empty()) {
                    tenant_list_query test_query;
                    test_query.search = q->search;
                    test_query.status = q->status;
                    test_query.types = std::vector<std::string>{std::string(test_tenant_type)};
                    test_query.limit = 1;
                    hidden = tenants_page(req, session, test_query).total;
                }
                std::map<std::string, tenant_setup> setups;
                bool setup_unavailable = false;
                try {
                    const auto runs = setups_of(req, session, ids_of(page.tenants));
                    setups = runs.setups;
                    if (!runs.complete)
                        BOOST_LOG_SEV(lg(), warn) << "The provisioning run read reached its "
                                                     "limit; the oldest runs are not shown.";
                } catch (const std::exception& e) {
                    BOOST_LOG_SEV(lg(), warn)
                        << "The provisioning runs were not read: " << e.what();
                    setup_unavailable = true;
                } catch (const rr::refusal& r) {
                    BOOST_LOG_SEV(lg(), warn)
                        << "The provisioning runs were not read: " << r.message;
                    setup_unavailable = true;
                }
                return http_response::json(
                    page_json(page.tenants, setups, page.total, setup_unavailable, hidden));
            });
        });

    add(router->get("/api/overview"),
        "Read the state of the deployment's tenants for the system administrator's home",
        [&session](const http_request& req) {
            return guarded<messaging::list_tenants_request>([&] {
                deployment_access::require_read(req, session, "The deployment overview");
                auto shown = tenant_type_codes(req, session);
                std::erase_if(shown, [](const std::string& code) {
                    return code == system_tenant_type || code == test_tenant_type;
                });
                const auto count = [&](std::string status, std::string type) {
                    tenant_list_query q;
                    q.status = std::move(status);
                    q.type = std::move(type);
                    q.types = shown;
                    q.limit = 1;
                    return tenants_page(req, session, q).total;
                };
                overview o;
                o.in_service = count("active", "");
                o.on_evaluation = count("", "evaluation");
                const auto bootstrapping = count("bootstrapping", "");
                tenant_list_query suspended_query;
                suspended_query.status = "suspended";
                suspended_query.types = shown;
                suspended_query.limit = overview_attention;
                const auto suspended = tenants_page(req, session, suspended_query);
                tenant_list_query first_query;
                first_query.types = shown;
                first_query.limit = overview_tenants;
                const auto first = tenants_page(req, session, first_query);

                std::vector<attention_entry> failed_setups;
                try {
                    workflow::list_workflow_instance_summaries_request recent_msg;
                    recent_msg.limit = overview_activity;
                    const auto recent = provisioning_runs(req, session, recent_msg).instances;
                    workflow::list_workflow_instance_summaries_request failed_msg;
                    failed_msg.limit = overview_attention;
                    failed_msg.status_filter = "failed";
                    const auto failed = provisioning_runs(req, session, failed_msg).instances;
                    std::vector<boost::uuids::uuid> ids;
                    for (const auto* runs : {&recent, &failed})
                        for (const auto& run : *runs) {
                            const auto id = run_target_of(run.target_id);
                            if (id && std::ranges::find(ids, *id) == ids.end())
                                ids.push_back(*id);
                        }
                    std::map<std::string, tenant> by_id;
                    if (!ids.empty()) {
                        tenant_list_query named_query;
                        named_query.ids = ids;
                        named_query.types = shown;
                        named_query.limit = static_cast<std::uint32_t>(ids.size());
                        for (const auto& t : tenants_page(req, session, named_query).tenants)
                            by_id[boost::uuids::to_string(t.id)] = t;
                    }
                    std::map<std::string, int> done;
                    const auto done_of = [&](const std::string& instance_id) {
                        const auto found = done.find(instance_id);
                        if (found != done.end())
                            return found->second;
                        return done[instance_id] = run_steps_done(req, session, instance_id);
                    };
                    std::vector<activity_entry> activity;
                    for (const auto& run : recent) {
                        const auto t = by_id.find(run.target_id);
                        if (t == by_id.end())
                            continue;
                        activity.push_back({run.id,
                                            t->second.name,
                                            run.status,
                                            done_of(run.id),
                                            run.step_count,
                                            run.error,
                                            run.completed_at.value_or(run.created_at)});
                    }
                    std::set<std::string> seen;
                    for (const auto& run : failed) {
                        const auto t = by_id.find(run.target_id);
                        if (t == by_id.end() || t->second.status != "bootstrapping" ||
                            seen.contains(t->first))
                            continue;
                        seen.insert(t->first);
                        failed_setups.push_back(
                            {t->second,
                             tenant_setup{
                                 run.id, run.status, done_of(run.id), run.step_count, run.error},
                             "setup-failed"});
                    }
                    o.activity = std::move(activity);
                } catch (const std::exception& e) {
                    BOOST_LOG_SEV(lg(), warn)
                        << "The provisioning runs were not read: " << e.what();
                    failed_setups.clear();
                    o.activity_unavailable = true;
                } catch (const rr::refusal& r) {
                    BOOST_LOG_SEV(lg(), warn)
                        << "The provisioning runs were not read: " << r.message;
                    failed_setups.clear();
                    o.activity_unavailable = true;
                }
                try {
                    o.setups = setups_of(req, session, ids_of(first.tenants)).setups;
                } catch (const std::exception& e) {
                    BOOST_LOG_SEV(lg(), warn)
                        << "The first tenants' setups were not read: " << e.what();
                } catch (const rr::refusal& r) {
                    BOOST_LOG_SEV(lg(), warn)
                        << "The first tenants' setups were not read: " << r.message;
                }
                const auto stalled = static_cast<std::uint64_t>(failed_setups.size());
                o.setting_up = bootstrapping > stalled ? bootstrapping - stalled : 0;
                o.attention = std::move(failed_setups);
                for (const auto& t : suspended.tenants)
                    o.attention.push_back({t, std::nullopt, "suspended"});
                o.tenants = first.tenants;
                o.total_count = first.total;
                return http_response::json(overview_json(o));
            });
        });

    add(router->get("/api/tenants/{code}"),
        "Read one tenant of the deployment and the run that set it up",
        [&session](const http_request& req) {
            return guarded<messaging::get_tenant_request>([&] {
                deployment_access::require_read(req, session, "A tenant of the deployment");
                const auto t =
                    tenant_roster_routes::known_tenant(req, session, req.get_path_param("code"));
                std::optional<tenant_setup> setup;
                bool setup_unavailable = false;
                try {
                    setup = setup_for(setups_of(req, session, ids_of({t})).setups, t);
                } catch (const std::exception& e) {
                    BOOST_LOG_SEV(lg(), warn)
                        << "The provisioning runs were not read: " << e.what();
                    setup_unavailable = true;
                } catch (const rr::refusal& r) {
                    BOOST_LOG_SEV(lg(), warn)
                        << "The provisioning runs were not read: " << r.message;
                    setup_unavailable = true;
                }
                return http_response::json(detail_json(t, setup, setup_unavailable));
            });
        });

    add(router->delete_("/api/tenants/{code}"),
        "Remove one tenant of the deployment",
        [&session](const http_request& req) {
            return guarded<messaging::delete_tenant_request>([&] {
                deployment_access::require_read(req, session, "A tenant of the deployment");
                const auto code = req.get_path_param("code");
                const auto body = json::read_expected<confirm_removal_body>(req.body);
                if (!body || body->confirmCode != code)
                    return rr::refused(http_status::bad_request,
                                       "invalid-request",
                                       "Type the tenant code to confirm the removal.");
                const auto t = tenant_roster_routes::known_tenant(req, session, code);
                if (boost::uuids::to_string(t.id) == rr::system_tenant_id)
                    return rr::refused(
                        http_status::conflict,
                        "conflict",
                        "The system tenant holds the deployment itself and cannot be removed.");
                messaging::delete_tenant_request msg;
                msg.removal.key.code = code;
                const auto reply = ores::http::forwarding::forward(req, session, msg);
                using ores::utility::domain::outcome;
                switch (reply.result.outcome) {
                    case outcome::ok:
                        BOOST_LOG_SEV(lg(), info) << "Tenant removed: " << code;
                        return rr::no_content();
                    case outcome::missing:
                        return rr::refused(
                            http_status::not_found, "not-found", "No tenant has this code.");
                    case outcome::denied:
                        return rr::refused(http_status::forbidden,
                                           "forbidden",
                                           reply.result.message.empty() ?
                                               "You may not remove tenants." :
                                               reply.result.message);
                    default:
                        return rr::refused(http_status::conflict,
                                           "conflict",
                                           reply.result.message.empty() ?
                                               "The tenant was not removed." :
                                               reply.result.message);
                }
            });
        });
}

}
