/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.wt/service/session_manager.hpp"
#include "ores.wt/service/application_context.hpp"
#include "ores.logging/make_logger.hpp"
#include <boost/asio/ip/address.hpp>

namespace {

const std::string logger_name = "ores.wt.service.session_manager";

auto& lg() {
    using namespace ores::logging;
    static auto instance = make_logger(logger_name);
    return instance;
}

}

namespace ores::wt::service {

session_manager::session_manager() = default;

login_result session_manager::login(const std::string& username,
                                    const std::string& password,
                                    const std::string& client_ip) {
    using namespace ores::logging;

    login_result result;

    auto& ctx = application_context::instance();
    if (!ctx.is_initialized()) {
        result.error_message = "Application not initialized";
        BOOST_LOG_SEV(lg(), error) << result.error_message;
        return result;
    }

    try {
        boost::asio::ip::address ip;
        try {
            ip = boost::asio::ip::make_address(client_ip);
        } catch (...) {
            ip = boost::asio::ip::make_address("127.0.0.1");
        }

        auto account = ctx.account_service().login(username, password, ip);

        auto login_info = ctx.account_service().get_login_info(account.id);

        session_data session;
        session.account_id = account.id;
        session.username = account.username;
        session.email = account.email;
        session_ = session;

        result.success = true;
        result.account_id = account.id;
        result.username = account.username;
        result.email = account.email;
        result.password_reset_required = login_info.password_reset_required;

        BOOST_LOG_SEV(lg(), info) << "User '" << username << "' logged in from "
                                  << client_ip;

    } catch (const std::runtime_error& e) {
        result.error_message = e.what();
        BOOST_LOG_SEV(lg(), warn) << "Login failed for '" << username << "': "
                                  << e.what();
    }

    return result;
}

void session_manager::logout() {
    using namespace ores::logging;

    if (session_) {
        auto& ctx = application_context::instance();
        if (ctx.is_initialized()) {
            try {
                ctx.account_service().logout(session_->account_id);
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), warn) << "Logout error: " << e.what();
            }
        }
        BOOST_LOG_SEV(lg(), info) << "User '" << session_->username
                                  << "' logged out";
        session_.reset();
    }
}

bool session_manager::has_permission(const std::string& permission) const {
    if (!session_) {
        return false;
    }

    auto& ctx = application_context::instance();
    if (!ctx.is_initialized()) {
        return false;
    }

    return ctx.authorization_service().has_permission(
        session_->account_id, permission);
}

login_result session_manager::create_bootstrap_admin(
    const std::string& username,
    const std::string& email,
    const std::string& password) {
    using namespace ores::logging;

    login_result result;

    auto& ctx = application_context::instance();
    if (!ctx.is_initialized()) {
        result.error_message = "Application not initialized";
        return result;
    }

    if (!ctx.is_bootstrap_mode()) {
        result.error_message = "System is not in bootstrap mode";
        return result;
    }

    try {
        auto account = ctx.account_setup_service().create_account(
            username, email, password, "bootstrap");

        auto admin_role = ctx.authorization_service().find_role_by_name("Admin");
        if (admin_role) {
            ctx.authorization_service().assign_role(
                account.id, admin_role->id, "bootstrap");
        }

        session_data session;
        session.account_id = account.id;
        session.username = account.username;
        session.email = account.email;
        session_ = session;

        result.success = true;
        result.account_id = account.id;
        result.username = account.username;
        result.email = account.email;

        BOOST_LOG_SEV(lg(), info) << "Bootstrap admin '" << username
                                  << "' created successfully";

    } catch (const std::exception& e) {
        result.error_message = e.what();
        BOOST_LOG_SEV(lg(), error) << "Failed to create bootstrap admin: "
                                   << e.what();
    }

    return result;
}

}
