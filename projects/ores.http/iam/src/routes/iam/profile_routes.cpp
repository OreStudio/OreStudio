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
#include "ores.http/routes/iam/profile_routes.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http/routes/iam/access_read_routes.hpp"
#include "ores.http/routes/iam/account_view.hpp"
#include "ores.iam.api/messaging/account_contact_information_protocol.hpp"
#include "ores.iam.api/messaging/account_operations_protocol.hpp"
#include "ores.iam.api/messaging/account_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <cstdint>
#include <functional>
#include <limits>
#include <utility>

namespace ores::http::routes::iam {

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;
namespace json = ores::utility::codec::json;
namespace messaging = ores::iam::messaging;
namespace rr = ores::http::net::record_routes;
using ores::http::forwarding::guarded;
using ores::http::net::record_routes::refused;
using ores::utility::domain::outcome;
using ores::utility::domain::precondition;
using ores::utility::domain::precondition_kind;

namespace {

http_response failed_operation(const std::string& message) {
    return refused(http_status::conflict, "invalid-request", message);
}

http_response moved_record() {
    return refused(http_status::conflict,
                   "conflict",
                   "This record was saved by somebody else since you opened it.");
}

/// A write's answer as the screen reads it: what the write decided, and the
/// record as written, or null.
template <typename Record>
http_response decided(const ores::utility::domain::result& result,
                      const std::optional<Record>& record,
                      std::string_view name) {
    return http_response::json("{\"result\":" + json::write(result) + ",\"" + std::string(name) +
                               "\":" + account_view::json_of(record) + "}");
}

std::optional<ores::iam::domain::account_contact_information>
contact_of(const http_request& req, nats_client& session, const std::string& account_id) {
    messaging::list_by_account_id_account_contact_informations_request list;
    list.account_id = boost::uuids::string_generator()(account_id);
    list.limit = 1;
    const auto reply = ores::http::forwarding::forward(req, session, list);
    if (reply.result.outcome != outcome::ok)
        throw rr::refusal{http_status::conflict, "invalid-request", reply.result.message};
    if (reply.account_contact_informations.empty())
        return std::nullopt;
    return reply.account_contact_informations.front();
}

}

std::optional<profile_body> profile_routes::read_profile(const std::string& body) {
    auto profile = json::read_expected<profile_body>(body.empty() ? "{}" : body);
    if (!profile)
        return std::nullopt;
    return std::move(*profile);
}

std::optional<contact_body> profile_routes::read_contact(const std::string& body) {
    auto contact = json::read_expected<contact_body>(body.empty() ? "{}" : body);
    if (!contact ||
        (contact->version &&
         (*contact->version < 0 || *contact->version > std::numeric_limits<std::uint32_t>::max())))
        return std::nullopt;
    return std::move(*contact);
}

bool profile_routes::moved(int current, const std::optional<std::string>& claimed) {
    return claimed && !claimed->empty() && std::to_string(current) != *claimed;
}

void profile_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    const auto add = [&](ores::http::net::route_builder builder,
                         std::string summary,
                         std::function<http_response(const http_request&)> handle) {
        const auto built =
            builder.summary(std::move(summary))
                .tags({"access"})
                .auth_required()
                .handler([handle = std::move(handle)](const http_request& req)
                             -> boost::asio::awaitable<http_response> { co_return handle(req); })
                .build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->put("/api/me/profile"),
        "Write the caller's own profile",
        [&session](const http_request& req) {
            const auto profile = read_profile(req.body);
            if (!profile)
                return refused(http_status::bad_request,
                               "invalid-request",
                               "The profile fields must be text.");
            return guarded<messaging::update_self_account_request>([&] {
                if (profile->expectedVersion && !profile->expectedVersion->empty()) {
                    const auto own = ores::http::forwarding::forward(
                        req, session, messaging::get_my_account_request{});
                    if (own.result.outcome != outcome::ok)
                        return failed_operation(own.result.message);
                    if (own.account && moved(own.account->version, profile->expectedVersion))
                        return moved_record();
                }
                messaging::update_self_account_request write;
                write.full_name = profile->fullName.value_or("");
                write.job_title = profile->jobTitle.value_or("");
                write.image_id = profile->imageId.value_or("");
                write.change_reason_code = profile->reasonCode.value_or("");
                write.change_commentary = profile->commentary.value_or("");
                const auto reply = ores::http::forwarding::forward(req, session, write);
                return decided(reply.result, reply.account, "account");
            });
        });

    add(router->put("/api/accounts/{username}/profile"),
        "Write one account's profile, as an administrator",
        [&session](const http_request& req) {
            const auto profile = read_profile(req.body);
            if (!profile)
                return refused(http_status::bad_request,
                               "invalid-request",
                               "The profile fields must be text.");
            const auto username = req.get_path_param("username");
            return guarded<messaging::update_account_request>([&] {
                const auto read = ores::http::forwarding::forward(
                    req, session, messaging::get_account_request{messaging::account_key{username}});
                if (!read.account)
                    return refused(
                        http_status::not_found, "not-found", "No account has this username.");
                const auto& account = *read.account;
                if (moved(account.version, profile->expectedVersion))
                    return moved_record();
                const auto reference = [](const std::optional<boost::uuids::uuid>& id) {
                    return id && !id->is_nil() ? boost::uuids::to_string(*id) : std::string{};
                };
                messaging::update_account_request write;
                write.account_id = boost::uuids::to_string(account.id);
                write.email = account.email;
                write.full_name = profile->fullName.value_or("");
                write.default_party_id = reference(account.default_party_id);
                write.job_title = profile->jobTitle.value_or("");
                write.reports_to_account_id = reference(account.reports_to_account_id);
                write.image_id = profile->imageId.value_or("");
                write.change_reason_code = profile->reasonCode.value_or("");
                write.change_commentary = profile->commentary.value_or("");
                const auto reply = ores::http::forwarding::forward(req, session, write);
                if (!reply.success)
                    return failed_operation(reply.message);
                return http_response::json(R"({"success":true})");
            });
        });

    add(router->get("/api/me/contact-information"),
        "Read the caller's own contact record",
        [&session](const http_request& req) {
            return guarded<messaging::get_my_account_contact_information_request>([&] {
                const auto reply = ores::http::forwarding::forward(
                    req, session, messaging::get_my_account_contact_information_request{});
                if (reply.result.outcome != outcome::ok)
                    return failed_operation(reply.result.message);
                return http_response::json(
                    "{\"contact\":" + account_view::json_of(reply.account_contact_information) +
                    "}");
            });
        });

    add(router->put("/api/me/contact-information"),
        "Write the caller's own contact record",
        [&session](const http_request& req) {
            const auto contact = read_contact(req.body);
            if (!contact)
                return refused(http_status::bad_request,
                               "invalid-request",
                               "The contact fields must be text.");
            return guarded<messaging::update_self_account_contact_information_request>([&] {
                messaging::update_self_account_contact_information_request write;
                write.street_line_1 = contact->streetLine1.value_or("");
                write.street_line_2 = contact->streetLine2.value_or("");
                write.city = contact->city.value_or("");
                write.state = contact->state.value_or("");
                write.country_code = contact->countryCode.value_or("");
                write.postal_code = contact->postalCode.value_or("");
                write.phone = contact->phone.value_or("");
                write.email = contact->email.value_or("");
                write.web_page = contact->webPage.value_or("");
                write.change_reason_code = contact->reasonCode.value_or("");
                write.change_commentary = contact->commentary.value_or("");
                const auto reply = ores::http::forwarding::forward(req, session, write);
                return decided(reply.result, reply.account_contact_information, "contact");
            });
        });

    add(router->get("/api/accounts/{accountId}/contact-information"),
        "Read one account's contact record, as an administrator",
        [&session](const http_request& req) {
            const auto account = req.get_path_param("accountId");
            if (!access_read_routes::is_uuid(account))
                return refused(
                    http_status::not_found, "not-found", "No account has this identifier.");
            return guarded<messaging::list_by_account_id_account_contact_informations_request>([&] {
                return http_response::json(
                    "{\"contact\":" + account_view::json_of(contact_of(req, session, account)) +
                    "}");
            });
        });

    add(router->put("/api/accounts/{accountId}/contact-information"),
        "Write one account's contact record, as an administrator",
        [&session](const http_request& req) {
            const auto account = req.get_path_param("accountId");
            if (!access_read_routes::is_uuid(account))
                return refused(
                    http_status::not_found, "not-found", "No account has this identifier.");
            const auto contact = read_contact(req.body);
            if (!contact)
                return refused(http_status::bad_request,
                               "invalid-request",
                               "Send the contact fields and the version the panel read.");
            return guarded<messaging::put_account_contact_information_request>([&] {
                const auto current = contact_of(req, session, account);
                messaging::put_account_contact_information_request put;
                auto& write = put.change.write;
                // The record's identifier is the route's, never the browser's: the
                // account's current record is reused, so a claim of no record finds
                // the row the panel did not see and is refused.
                write.id = current ? current->id : boost::uuids::random_generator()();
                write.account_id = boost::uuids::string_generator()(account);
                write.street_line_1 = contact->streetLine1.value_or("");
                write.street_line_2 = contact->streetLine2.value_or("");
                write.city = contact->city.value_or("");
                write.state = contact->state.value_or("");
                write.country_code = contact->countryCode.value_or("");
                write.postal_code = contact->postalCode.value_or("");
                write.phone = contact->phone.value_or("");
                write.email = contact->email.value_or("");
                write.web_page = contact->webPage.value_or("");
                put.change.precondition =
                    contact->version ?
                        precondition{precondition_kind::must_match_version,
                                     static_cast<std::uint32_t>(*contact->version)} :
                        precondition{precondition_kind::must_not_exist, std::nullopt};
                put.intent.reason_code = contact->reasonCode.value_or("");
                put.intent.commentary = contact->commentary.value_or("");
                const auto reply = ores::http::forwarding::forward(req, session, put);
                return decided(reply.result, reply.account_contact_information, "contact");
            });
        });
}

}
