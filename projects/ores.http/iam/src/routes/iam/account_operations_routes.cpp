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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_http_route_operation_implementation.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.http/routes/iam/account_operations_routes.hpp"
#include "ores.iam.api/messaging/account_operations_protocol.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/session_expired_error.hpp"
#include "ores.nats/service/timeouts.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <exception>
#include <memory>
#include <rfl/json.hpp>
#include <string>
#include <utility>

namespace ores::http::routes::iam {

using namespace logging;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::nats::service::nats_client;
namespace messaging = ores::iam::messaging;

namespace {

/**
 * @brief The HTTP response a service error names.
 *
 * The service answers a refused caller with a status in its reply header
 * rather than a body, so the gateway reads that status and maps it. An error
 * it does not know is the caller's to see as unauthorised, which is what the
 * service means by any code it does not spell out here.
 */
http_response error_response(const std::string& error) {
    if (error == "forbidden") {
        return http_response::forbidden(error);
    }
    if (error == "bad_request") {
        return http_response::bad_request(error);
    }
    return http_response::unauthorized(error);
}

/**
 * @brief Forward one canonical request and map the reply to a response.
 *
 * The caller's own bearer token is taken from the HTTP request and delegated
 * on the NATS call, so the service validates that caller and its own
 * permission check governs. The gateway authorises nobody and decides
 * nothing; it states the subject the request already carries.
 */
template <typename Request>
boost::asio::awaitable<http_response>
forward(const http_request& req, nats_client& session, const Request& msg) {
    auto delegated = session.with_delegation(req.get_bearer_token().value_or(std::string{}));
    try {
        const auto reply =
            delegated.authenticated_request(msg.nats_subject,
                                            ores::nats::default_wire_codec().encode(msg),
                                            ores::nats::service::default_request_timeout);
        if (const auto it = reply.headers.find(std::string(ores::nats::headers::x_error));
            it != reply.headers.end()) {
            co_return error_response(it->second);
        }
        const auto decoded =
            ores::nats::default_wire_codec().decode<typename Request::response_type>(reply.data);
        if (!decoded) {
            co_return http_response::internal_error(std::string("Failed to decode response: ") +
                                                    decoded.error().what());
        }
        co_return http_response::json(rfl::json::write(*decoded));
    } catch (const ores::nats::service::session_expired_error& e) {
        co_return http_response::unauthorized(e.what());
    } catch (const std::exception& e) {
        co_return http_response::internal_error(e.what());
    }
}

} // namespace

boost::asio::awaitable<http_response>
account_operations_routes::handle_save_account(const http_request& req, nats_client& session) {
    using request_type = messaging::save_account_request;
    try {
        const auto parsed = rfl::json::read<request_type>(req.body);
        if (!parsed) {
            co_return http_response::bad_request("Invalid request body");
        }
        co_return co_await forward(req, session, *parsed);
    } catch (const std::exception& e) {
        co_return http_response::bad_request(e.what());
    }
}

boost::asio::awaitable<http_response>
account_operations_routes::handle_update_account(const http_request& req, nats_client& session) {
    using request_type = messaging::update_account_request;
    try {
        const auto parsed = rfl::json::read<request_type>(req.body);
        if (!parsed) {
            co_return http_response::bad_request("Invalid request body");
        }
        co_return co_await forward(req, session, *parsed);
    } catch (const std::exception& e) {
        co_return http_response::bad_request(e.what());
    }
}

boost::asio::awaitable<http_response>
account_operations_routes::handle_delete_account(const http_request& req, nats_client& session) {
    using request_type = messaging::delete_account_request;
    try {
        const auto parsed = rfl::json::read<request_type>(req.body);
        if (!parsed) {
            co_return http_response::bad_request("Invalid request body");
        }
        co_return co_await forward(req, session, *parsed);
    } catch (const std::exception& e) {
        co_return http_response::bad_request(e.what());
    }
}

boost::asio::awaitable<http_response>
account_operations_routes::handle_lock_account(const http_request& req, nats_client& session) {
    using request_type = messaging::lock_account_request;
    try {
        const auto parsed = rfl::json::read<request_type>(req.body);
        if (!parsed) {
            co_return http_response::bad_request("Invalid request body");
        }
        co_return co_await forward(req, session, *parsed);
    } catch (const std::exception& e) {
        co_return http_response::bad_request(e.what());
    }
}

boost::asio::awaitable<http_response>
account_operations_routes::handle_unlock_account(const http_request& req, nats_client& session) {
    using request_type = messaging::unlock_account_request;
    try {
        const auto parsed = rfl::json::read<request_type>(req.body);
        if (!parsed) {
            co_return http_response::bad_request("Invalid request body");
        }
        co_return co_await forward(req, session, *parsed);
    } catch (const std::exception& e) {
        co_return http_response::bad_request(e.what());
    }
}

boost::asio::awaitable<http_response>
account_operations_routes::handle_reset_password(const http_request& req, nats_client& session) {
    using request_type = messaging::reset_password_request;
    try {
        const auto parsed = rfl::json::read<request_type>(req.body);
        if (!parsed) {
            co_return http_response::bad_request("Invalid request body");
        }
        co_return co_await forward(req, session, *parsed);
    } catch (const std::exception& e) {
        co_return http_response::bad_request(e.what());
    }
}

boost::asio::awaitable<http_response>
account_operations_routes::handle_update_my_email(const http_request& req, nats_client& session) {
    using request_type = messaging::update_my_email_request;
    try {
        const auto parsed = rfl::json::read<request_type>(req.body);
        if (!parsed) {
            co_return http_response::bad_request("Invalid request body");
        }
        co_return co_await forward(req, session, *parsed);
    } catch (const std::exception& e) {
        co_return http_response::bad_request(e.what());
    }
}

boost::asio::awaitable<http_response>
account_operations_routes::handle_change_password(const http_request& req, nats_client& session) {
    using request_type = messaging::change_password_request_typed;
    try {
        const auto parsed = rfl::json::read<request_type>(req.body);
        if (!parsed) {
            co_return http_response::bad_request("Invalid request body");
        }
        co_return co_await forward(req, session, *parsed);
    } catch (const std::exception& e) {
        co_return http_response::bad_request(e.what());
    }
}

void account_operations_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    BOOST_LOG_SEV(lg(), info) << "Registering account_operations routes";

    auto save_account_route = router->post("/api/v1/iam/accounts/save")
                                  .summary("Save account")
                                  .description("Forwards to the iam.v1.accounts.save operation.")
                                  .tags({"iam"})
                                  .auth_required()
                                  .body<messaging::save_account_request>()
                                  .response<messaging::save_account_response>()
                                  .handler([&session](const http_request& req) {
                                      return handle_save_account(req, session);
                                  });
    router->add_route(save_account_route.build());
    registry->register_route(save_account_route.build());

    auto update_account_route =
        router->post("/api/v1/iam/accounts/update")
            .summary("Update account")
            .description("Forwards to the iam.v1.accounts.update operation.")
            .tags({"iam"})
            .auth_required()
            .body<messaging::update_account_request>()
            .response<messaging::update_account_response>()
            .handler([&session](const http_request& req) {
                return handle_update_account(req, session);
            });
    router->add_route(update_account_route.build());
    registry->register_route(update_account_route.build());

    auto delete_account_route =
        router->post("/api/v1/iam/accounts/delete")
            .summary("Delete account")
            .description("Forwards to the iam.v1.accounts.delete operation.")
            .tags({"iam"})
            .auth_required()
            .body<messaging::delete_account_request>()
            .response<messaging::delete_account_response>()
            .handler([&session](const http_request& req) {
                return handle_delete_account(req, session);
            });
    router->add_route(delete_account_route.build());
    registry->register_route(delete_account_route.build());

    auto lock_account_route = router->post("/api/v1/iam/accounts/lock")
                                  .summary("Lock account")
                                  .description("Forwards to the iam.v1.accounts.lock operation.")
                                  .tags({"iam"})
                                  .auth_required()
                                  .body<messaging::lock_account_request>()
                                  .response<messaging::lock_account_response>()
                                  .handler([&session](const http_request& req) {
                                      return handle_lock_account(req, session);
                                  });
    router->add_route(lock_account_route.build());
    registry->register_route(lock_account_route.build());

    auto unlock_account_route =
        router->post("/api/v1/iam/accounts/unlock")
            .summary("Unlock account")
            .description("Forwards to the iam.v1.accounts.unlock operation.")
            .tags({"iam"})
            .auth_required()
            .body<messaging::unlock_account_request>()
            .response<messaging::unlock_account_response>()
            .handler([&session](const http_request& req) {
                return handle_unlock_account(req, session);
            });
    router->add_route(unlock_account_route.build());
    registry->register_route(unlock_account_route.build());

    auto reset_password_route =
        router->post("/api/v1/iam/accounts/reset-password")
            .summary("Reset password")
            .description("Forwards to the iam.v1.accounts.reset-password operation.")
            .tags({"iam"})
            .auth_required()
            .body<messaging::reset_password_request>()
            .response<messaging::reset_password_response>()
            .handler([&session](const http_request& req) {
                return handle_reset_password(req, session);
            });
    router->add_route(reset_password_route.build());
    registry->register_route(reset_password_route.build());

    auto update_my_email_route =
        router->post("/api/v1/iam/accounts/update-email")
            .summary("Update my email")
            .description("Forwards to the iam.v1.accounts.update-email operation.")
            .tags({"iam"})
            .auth_required()
            .body<messaging::update_my_email_request>()
            .response<messaging::update_my_email_response>()
            .handler([&session](const http_request& req) {
                return handle_update_my_email(req, session);
            });
    router->add_route(update_my_email_route.build());
    registry->register_route(update_my_email_route.build());

    auto change_password_route =
        router->post("/api/v1/iam/accounts/change-password")
            .summary("Change password")
            .description("Forwards to the iam.v1.accounts.change-password operation.")
            .tags({"iam"})
            .auth_required()
            .body<messaging::change_password_request_typed>()
            .response<messaging::change_password_response>()
            .handler([&session](const http_request& req) {
                return handle_change_password(req, session);
            });
    router->add_route(change_password_route.build());
    registry->register_route(change_password_route.build());

    BOOST_LOG_SEV(lg(), info) << "account_operations routes registered: " << 8 << " endpoint(s)";
}

}
