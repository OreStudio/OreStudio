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
#include "ores.http/routes/iam/provisioning_routes.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.iam.api/messaging/seed_profile_parameter_protocol.hpp"
#include "ores.iam.api/messaging/seed_profile_protocol.hpp"
#include "ores.iam.api/messaging/seed_profile_step_protocol.hpp"
#include "ores.refdata.api/messaging/party_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <map>
#include <stdexcept>
#include <tuple>
#include <type_traits>

namespace ores::http::routes::iam {

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;
namespace json = ores::utility::codec::json;
namespace messaging = ores::iam::messaging;
namespace refdata = ores::refdata::messaging;
namespace rr = ores::http::net::record_routes;

namespace {

struct provision_tenant_body final {
    std::string profileCode;
    std::string tenantCode;
    std::string tenantName;
    std::string tenantHostname;
    std::optional<std::string> tenantDescription;
    std::string adminUsername;
    std::string adminEmail;
    std::string adminPassword;
    std::optional<std::map<std::string, std::string>> parameters;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<provision_tenant_body>) {
    using ores::utility::codec::field;
    using b = provision_tenant_body;
    return std::tuple{field{"profileCode", &b::profileCode},
                      field{"tenantCode", &b::tenantCode},
                      field{"tenantName", &b::tenantName},
                      field{"tenantHostname", &b::tenantHostname},
                      field{"tenantDescription", &b::tenantDescription},
                      field{"adminUsername", &b::adminUsername},
                      field{"adminEmail", &b::adminEmail},
                      field{"adminPassword", &b::adminPassword},
                      field{"parameters", &b::parameters}};
}

struct provision_party_body final {
    std::string fullName;
    std::string shortCode;
    std::optional<std::string> lei;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<provision_party_body>) {
    using ores::utility::codec::field;
    return std::tuple{field{"fullName", &provision_party_body::fullName},
                      field{"shortCode", &provision_party_body::shortCode},
                      field{"lei", &provision_party_body::lei}};
}

/// How many parties one page of the root search reads, as the BFF read them.
constexpr std::uint32_t party_page = 500;

/// How many starting points the journey reads, as the BFF read them. A
/// deployment holds a handful, so one page holds them all.
constexpr std::uint32_t profile_page = 50;

/// How many steps or parameters one starting point's read takes, as the BFF
/// read them. A profile declares far fewer.
constexpr std::uint32_t profile_child_page = 200;

/// A JSON list column's texts, refused when it holds anything else.
std::vector<std::string> texts_or_throw(const std::string& column, std::string_view what) {
    auto texts = provisioning_routes::texts_of(column);
    if (!texts)
        throw std::runtime_error(std::string(what) + " is not a JSON list of texts.");
    return *texts;
}

std::string choice_json(const provisioning_routes::seed_profile_reads& read) {
    const auto& p = read.profile;
    std::string out = "{\"code\":" + json::write(p.code);
    out += ",\"name\":" + json::write(p.name);
    out += ",\"summary\":" + json::write(p.summary);
    out += ",\"audience\":" + json::write(p.audience);
    out += ",\"bullets\":" + json::write(texts_or_throw(p.bullets_json, "A profile's bullets"));
    out += ",\"tenant\":{\"name\":" + json::write(p.tenant_name);
    out += ",\"code\":" + json::write(p.tenant_code);
    out += ",\"hostname\":" + json::write(p.tenant_hostname);
    out += ",\"adminUsername\":" + json::write(p.admin_username);
    out += ",\"adminEmail\":" + json::write(p.admin_email) + "}";
    out +=
        ",\"inheritsAdminPassword\":" + std::string(p.inherits_admin_password ? "true" : "false");
    out += ",\"forcePasswordChange\":" + std::string(p.force_password_change ? "true" : "false");
    out += ",\"order\":" + std::to_string(p.display_order);
    out += ",\"steps\":[";
    for (std::size_t i = 0; i < read.steps.size(); ++i) {
        const auto& s = read.steps[i];
        out += (i == 0 ? "{" : ",{");
        out += "\"kind\":" + json::write(s.step_kind);
        out += ",\"order\":" + std::to_string(s.display_order) + "}";
    }
    out += "],\"parameters\":[";
    for (std::size_t i = 0; i < read.parameters.size(); ++i) {
        const auto& m = read.parameters[i];
        out += (i == 0 ? "{" : ",{");
        out += "\"name\":" + json::write(m.name);
        out += ",\"label\":" + json::write(m.label);
        out += ",\"dataType\":" + json::write(m.data_type);
        out +=
            ",\"choices\":" + json::write(texts_or_throw(m.choices_json, "A parameter's choices"));
        out += ",\"defaultValue\":" + json::write(m.default_value);
        out += ",\"required\":" + std::string(m.is_required ? "true" : "false");
        out += ",\"hint\":" + json::write(m.description);
        out += ",\"order\":" + std::to_string(m.display_order) + "}";
    }
    return out + "]}";
}

/// Every party of the caller's own tenant, read a page at a time.
std::vector<ores::refdata::domain::party> every_party(const http_request& req,
                                                      nats_client& session) {
    std::vector<ores::refdata::domain::party> parties;
    for (std::uint32_t offset = 0;; offset += party_page) {
        refdata::list_parties_request msg;
        msg.offset = offset;
        msg.limit = party_page;
        const auto reply = ores::http::forwarding::forward(req, session, msg);
        rr::require_done(reply.result);
        parties.insert(parties.end(), reply.parties.begin(), reply.parties.end());
        if (reply.parties.empty() || parties.size() >= reply.total)
            return parties;
    }
}

std::string party_result_json(bool success,
                              const std::string& message,
                              const std::string& instance_id,
                              const std::string& party_id) {
    return "{\"success\":" + std::string(success ? "true" : "false") +
           ",\"message\":" + json::write(message) + ",\"instanceId\":" + json::write(instance_id) +
           ",\"partyId\":" + json::write(party_id) + "}";
}

}

std::optional<std::vector<std::string>> provisioning_routes::texts_of(const std::string& column) {
    if (column.empty())
        return std::vector<std::string>{};
    auto texts = json::read_expected<std::vector<std::string>>(column);
    if (!texts)
        return std::nullopt;
    return *texts;
}

std::string provisioning_routes::profiles_json(std::vector<seed_profile_reads> reads) {
    std::ranges::stable_sort(reads, [](const auto& left, const auto& right) {
        return std::tie(left.profile.display_order, left.profile.code) <
               std::tie(right.profile.display_order, right.profile.code);
    });
    std::string out = "{\"profiles\":[";
    for (std::size_t i = 0; i < reads.size(); ++i)
        out += (i == 0 ? "" : ",") + choice_json(reads[i]);
    return out + "]}";
}

std::optional<messaging::provision_tenant_command>
provisioning_routes::read_tenant_command(const std::string& body) {
    const auto read = json::read_expected<provision_tenant_body>(body);
    if (!read)
        return std::nullopt;
    const auto& b = *read;
    for (const auto* required : {&b.profileCode,
                                 &b.tenantCode,
                                 &b.tenantName,
                                 &b.tenantHostname,
                                 &b.adminUsername,
                                 &b.adminEmail,
                                 &b.adminPassword})
        if (required->empty())
            return std::nullopt;
    messaging::provision_tenant_command out;
    out.profile_code = b.profileCode;
    out.tenant_code = b.tenantCode;
    out.tenant_name = b.tenantName;
    out.tenant_hostname = b.tenantHostname;
    out.tenant_description = b.tenantDescription.value_or("");
    out.admin_username = b.adminUsername;
    out.admin_email = b.adminEmail;
    out.admin_password = b.adminPassword;
    for (const auto& [name, value] : b.parameters.value_or(std::map<std::string, std::string>{}))
        out.parameters.push_back(name + "=" + value);
    return out;
}

std::optional<boost::uuids::uuid>
provisioning_routes::root_of(const std::vector<ores::refdata::domain::party>& parties) {
    const auto root = std::ranges::find_if(parties, [](const auto& party) {
        return !party.parent_party_id && party.party_category != "System";
    });
    if (root == parties.end())
        return std::nullopt;
    return root->id;
}

void provisioning_routes::register_routes(
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

    add(router->get("/api/seed-profiles"),
        "Read the starting points a tenant is provisioned from",
        [&session](const http_request& req) {
            return ores::http::forwarding::guarded<messaging::list_seed_profiles_request>([&] {
                messaging::list_seed_profiles_request page_msg;
                page_msg.limit = profile_page;
                const auto page = ores::http::forwarding::forward(req, session, page_msg);
                rr::require_done(page.result);
                std::vector<seed_profile_reads> reads;
                for (const auto& profile : page.seed_profiles) {
                    messaging::list_by_seed_profile_id_seed_profile_steps_request steps_msg;
                    steps_msg.seed_profile_id = profile.id;
                    steps_msg.limit = profile_child_page;
                    const auto steps = ores::http::forwarding::forward(req, session, steps_msg);
                    rr::require_done(steps.result);
                    messaging::list_by_seed_profile_id_seed_profile_parameters_request params_msg;
                    params_msg.seed_profile_id = profile.id;
                    params_msg.limit = profile_child_page;
                    const auto params = ores::http::forwarding::forward(req, session, params_msg);
                    rr::require_done(params.result);
                    reads.push_back(
                        {profile, steps.seed_profile_steps, params.seed_profile_parameters});
                }
                return http_response::json(profiles_json(std::move(reads)));
            });
        });

    add(router->post("/api/provision-tenant"),
        "Provision a tenant from a starting point",
        [&session](const http_request& req) {
            const auto command = read_tenant_command(req.body);
            if (!command)
                return rr::refused(
                    http_status::bad_request,
                    "invalid-request",
                    "A profile code, the tenant fields and the administrator fields are required.");
            return ores::http::forwarding::guarded<messaging::provision_tenant_command>([&] {
                const auto reply = ores::http::forwarding::forward(req, session, *command);
                return http_response::json(
                    "{\"success\":" + std::string(reply.success ? "true" : "false") +
                    ",\"message\":" + json::write(reply.message) +
                    ",\"instanceId\":" + json::write(reply.instance_id) +
                    ",\"tenantId\":" + json::write(reply.tenant_id) +
                    ",\"accountId\":" + json::write(reply.account_id) + "}");
            });
        });

    add(router->post("/api/provision-party"),
        "Add a party to the caller's tenant and start the run that brings it to life",
        [&session](const http_request& req) {
            const auto body = json::read_expected<provision_party_body>(req.body);
            if (!body || body->fullName.empty() || body->shortCode.empty())
                return rr::refused(http_status::bad_request,
                                   "invalid-request",
                                   "The legal name and a short code are required.");
            return ores::http::forwarding::guarded<messaging::provision_party_command>([&] {
                const auto id = boost::uuids::random_generator()();
                refdata::put_party_request put;
                put.change.write.id = id;
                put.change.write.short_code = body->shortCode;
                put.change.write.full_name = body->fullName;
                put.change.write.party_category = "Operational";
                put.change.write.party_type = "Corporate";
                put.change.write.parent_party_id = root_of(every_party(req, session));
                put.change.write.business_center_code = "WRLD";
                put.change.write.status = "Inactive";
                put.change.precondition.kind =
                    ores::utility::domain::precondition_kind::must_not_exist;
                const auto created = ores::http::forwarding::forward(req, session, put);
                if (created.result.outcome != ores::utility::domain::outcome::ok)
                    return http_response::json(
                        party_result_json(false, created.result.message, "", ""));
                const auto reply = ores::http::forwarding::forward(
                    req,
                    session,
                    messaging::provision_party_command{
                        boost::uuids::to_string(id), "", body->lei.value_or("")});
                return http_response::json(party_result_json(
                    reply.success, reply.message, reply.instance_id, reply.party_id));
            });
        });
}

}
