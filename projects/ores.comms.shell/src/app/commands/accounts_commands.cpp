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
#include "ores.comms.shell/app/commands/accounts_commands.hpp"

#include <iomanip>
#include <map>
#include <ostream>
#include <sstream>
#include <functional>
#include <rfl/json.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <cli/cli.h>
#include "ores.platform/time/datetime.hpp"
#include "ores.iam/messaging/login_protocol.hpp"
#include "ores.iam/messaging/bootstrap_protocol.hpp"
#include "ores.iam/messaging/account_protocol.hpp"
#include "ores.iam/messaging/session_protocol.hpp"
#include "ores.iam/messaging/account_history_protocol.hpp"
#include "ores.iam/messaging/authorization_protocol.hpp"
#include "ores.iam/domain/account_table_io.hpp"  // IWYU pragma: keep.
#include "ores.iam/domain/login_info_table_io.hpp"  // IWYU pragma: keep.
#include "ores.iam/domain/account_version_table_io.hpp"  // IWYU pragma: keep.
#include "ores.comms.shell/app/commands/rbac_commands.hpp"

namespace ores::comms::shell::app::commands {

using namespace ores::logging;
using service::nats_session;

namespace {

std::string format_time(std::chrono::system_clock::time_point tp) {
    return ores::platform::time::datetime::format_time_point(tp);
}

std::string format_bytes(std::uint64_t bytes) {
    if (bytes >= 1024 * 1024) {
        std::ostringstream oss;
        oss << std::fixed << std::setprecision(2) << (bytes / (1024.0 * 1024.0)) << " MB";
        return oss.str();
    } else if (bytes >= 1024) {
        std::ostringstream oss;
        oss << std::fixed << std::setprecision(2) << (bytes / 1024.0) << " KB";
        return oss.str();
    }
    return std::to_string(bytes) + " B";
}

std::string format_duration(std::chrono::seconds dur) {
    auto hours = std::chrono::duration_cast<std::chrono::hours>(dur);
    auto mins = std::chrono::duration_cast<std::chrono::minutes>(dur - hours);
    if (hours.count() > 0) {
        return std::to_string(hours.count()) + "h " + std::to_string(mins.count()) + "m";
    }
    return std::to_string(mins.count()) + "m";
}

template<typename Response>
std::optional<Response> do_request(std::ostream& out, nats_session& session,
    const std::string& subject, const std::string& body) {
    try {
        auto reply = session.request(subject, body);
        auto data_str = std::string(
            reinterpret_cast<const char*>(reply.data.data()), reply.data.size());
        auto result = rfl::json::read<Response>(data_str);
        if (!result) {
            out << "✗ Failed to parse response" << std::endl;
            return std::nullopt;
        }
        return *result;
    } catch (const std::exception& e) {
        out << "✗ Request failed: " << e.what() << std::endl;
        return std::nullopt;
    }
}

template<typename Response>
std::optional<Response> do_auth_request(std::ostream& out, nats_session& session,
    const std::string& subject, const std::string& body) {
    try {
        auto reply = session.authenticated_request(subject, body);
        auto data_str = std::string(
            reinterpret_cast<const char*>(reply.data.data()), reply.data.size());
        auto result = rfl::json::read<Response>(data_str);
        if (!result) {
            out << "✗ Failed to parse response" << std::endl;
            return std::nullopt;
        }
        return *result;
    } catch (const std::exception& e) {
        out << "✗ Request failed: " << e.what() << std::endl;
        return std::nullopt;
    }
}

} // anonymous namespace

void accounts_commands::
register_commands(cli::Menu& root_menu, nats_session& session,
                  pagination_context& pagination) {
    auto accounts_menu =
        std::make_unique<cli::Menu>("accounts");

    accounts_menu->Insert("create", [&session](std::ostream& out,
            std::string principal, std::string password, std::string totp_secret,
            std::string email) {
        process_create_account(std::ref(out),
            std::ref(session), std::move(principal),
            std::move(password), std::move(totp_secret),
                std::move(email));
    }, "Create a new account (principal password totp_secret email) - principal is username@hostname or username");

    accounts_menu->Insert("list", [&session, &pagination](std::ostream& out) {
        process_list_accounts(std::ref(out), std::ref(session),
                              std::ref(pagination));
    }, "Retrieve accounts from the server (paginated)");

    // Register list callback for navigation
    pagination.register_list_callback("accounts",
        [&session, &pagination](std::ostream& out) {
            process_list_accounts(out, session, pagination);
        });

    accounts_menu->Insert("login", [&session](std::ostream& out,
            std::string principal, std::string password) {
        process_login(std::ref(out), std::ref(session), std::move(principal),
            std::move(password));
    }, "Login with principal (username@hostname or username) and password");

    accounts_menu->Insert("lock",
        [&session](std::ostream& out, std::string account_id) {
        process_lock_account(std::ref(out),
            std::ref(session),
            std::move(account_id));
    }, "Lock an account (account_id) - requires accounts:lock permission");

    accounts_menu->Insert("unlock",
        [&session](std::ostream& out, std::string account_id) {
        process_unlock_account(std::ref(out),
            std::ref(session),
            std::move(account_id));
    }, "Unlock a locked account (account_id) - requires accounts:unlock permission");

    accounts_menu->Insert("list-logins", [&session](std::ostream& out) {
        process_list_login_info(std::ref(out), std::ref(session));
    }, "Retrieve all login info records from the server");

    accounts_menu->Insert("logout", [&session](std::ostream& out) {
        process_logout(std::ref(out), std::ref(session));
    }, "Logout the current user");

    accounts_menu->Insert("roles", [&session](std::ostream& out,
            std::string account_id) {
        rbac_commands::process_get_account_roles(std::ref(out), std::ref(session),
            std::move(account_id));
    }, "List roles assigned to an account (account_id)");

    accounts_menu->Insert("assign-role", [&session](std::ostream& out,
            std::string account_id, std::string role_id) {
        rbac_commands::process_assign_role(std::ref(out), std::ref(session),
            std::move(account_id), std::move(role_id));
    }, "Assign a role to an account (account_id role_id | principal role_name)");

    accounts_menu->Insert("revoke-role", [&session](std::ostream& out,
            std::string account_id, std::string role_id) {
        rbac_commands::process_revoke_role(std::ref(out), std::ref(session),
            std::move(account_id), std::move(role_id));
    }, "Revoke a role from an account (account_id role_id | principal role_name)");

    accounts_menu->Insert("permissions", [&session](std::ostream& out,
            std::string account_id) {
        rbac_commands::process_get_account_permissions(std::ref(out), std::ref(session),
            std::move(account_id));
    }, "List effective permissions for an account (account_id)");

    // Session commands
    accounts_menu->Insert("sessions", [&session](std::ostream& out) {
        process_list_sessions(std::ref(out), std::ref(session));
    }, "List your session history");

    accounts_menu->Insert("sessions-for", [&session](std::ostream& out,
            std::string account_id) {
        process_list_sessions(std::ref(out), std::ref(session), std::move(account_id));
    }, "List sessions for an account (account_id) - requires accounts:read permission");

    accounts_menu->Insert("active-sessions", [&session](std::ostream& out) {
        process_active_sessions(std::ref(out), std::ref(session));
    }, "List your currently active sessions");

    accounts_menu->Insert("session-stats", [&session](std::ostream& out) {
        process_session_stats(std::ref(out), std::ref(session));
    }, "Show session statistics for the last 30 days");

    accounts_menu->Insert("session-stats-days", [&session](std::ostream& out, int days) {
        process_session_stats(std::ref(out), std::ref(session), days);
    }, "Show session statistics for the specified number of days");

    accounts_menu->Insert("history", [&session](std::ostream& out,
            std::string username) {
        process_get_account_history(std::ref(out), std::ref(session),
            std::move(username));
    }, "Get version history for an account by username");

    accounts_menu->Insert("info", [&session](std::ostream& out,
            std::string username) {
        process_account_info(std::ref(out), std::ref(session),
            std::move(username));
    }, "Show comprehensive account info (username) - details, roles, permissions");

    root_menu.Insert(std::move(accounts_menu));

    // Bootstrap command at root level (doesn't require authentication)
    root_menu.Insert("bootstrap", [&session](std::ostream& out,
            std::string principal, std::string password, std::string email) {
        process_bootstrap(std::ref(out), std::ref(session),
            std::move(principal), std::move(password), std::move(email));
    }, "Create initial admin (principal password email) - principal is username@hostname or username");

    // Top-level login/logout aliases for convenience
    root_menu.Insert("login", [&session](std::ostream& out,
            std::string principal, std::string password) {
        process_login(std::ref(out), std::ref(session), std::move(principal),
            std::move(password));
    }, "Login with principal (username@hostname or username) and password");

    root_menu.Insert("logout", [&session](std::ostream& out) {
        process_logout(std::ref(out), std::ref(session));
    }, "Logout the current user (alias for 'accounts logout')");
}

void accounts_commands::
process_list_accounts(std::ostream& out, nats_session& session,
                      pagination_context& pagination) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating list account request.";

    if (!session.is_logged_in()) {
        out << "✗ You must be logged in to list accounts." << std::endl;
        return;
    }

    auto& state = pagination.state_for("accounts");

    iam::messaging::get_accounts_request req;
    req.offset = state.current_offset;
    req.limit = pagination.page_size();

    auto result = do_auth_request<iam::messaging::get_accounts_response>(
        out, session, "ores.iam.v1.accounts.list", rfl::json::write(req));
    if (!result) return;

    state.total_count = result->total_available_count;
    pagination.set_last_entity("accounts");

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->accounts.size() << " accounts.";
    out << result->accounts << std::endl;

    // Display pagination info
    const auto page = (state.current_offset / pagination.page_size()) + 1;
    const auto total_pages = state.total_count > 0
        ? ((state.total_count + pagination.page_size() - 1) / pagination.page_size())
        : 1;
    out << "\nPage " << page << " of " << total_pages
        << " (" << result->accounts.size() << " of "
        << state.total_count << " total)" << std::endl;
}

void accounts_commands::
process_login(std::ostream& out, nats_session& session,
    std::string principal, std::string password) {
    iam::messaging::login_request req;
    req.principal = std::move(principal);
    req.password = std::move(password);

    auto result = do_request<iam::messaging::login_response>(
        out, session, "ores.iam.v1.auth.login", rfl::json::write(req));
    if (!result) return;

    if (!result->success) {
        out << "✗ Login failed: " << result->message << std::endl;
        return;
    }

    nats_session::login_info info;
    info.jwt = result->token;
    info.username = result->username;
    info.tenant_id = result->tenant_id;
    info.tenant_name = result->tenant_name;
    session.set_auth(std::move(info));

    out << "✓ Login successful!" << std::endl;
    out << "  User: " << result->username << std::endl;
    out << "  Tenant: " << result->tenant_name
        << " (" << result->tenant_id << ")" << std::endl;
}

void accounts_commands::
process_lock_account(std::ostream& out, nats_session& session,
    std::string account_id) {
    boost::uuids::uuid parsed_id;
    try {
        parsed_id = boost::lexical_cast<boost::uuids::uuid>(account_id);
    } catch (const boost::bad_lexical_cast&) {
        BOOST_LOG_SEV(lg(), error) << "Invalid account ID format: " << account_id;
        out << "✗ Invalid account ID format. Expected UUID." << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating lock account request for ID: "
                               << account_id;

    iam::messaging::lock_account_request req;
    req.account_ids = {parsed_id};

    auto result = do_auth_request<iam::messaging::lock_account_response>(
        out, session, "ores.iam.v1.accounts.lock", rfl::json::write(req));
    if (!result) return;

    if (result->results.empty()) {
        out << "✗ No results returned from server" << std::endl;
        return;
    }

    const auto& account_result = result->results[0];
    if (account_result.success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully locked account: " << account_id;
        out << "✓ Account locked successfully!" << std::endl
            << "  Account ID: " << account_id << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Failed to lock account: " << account_result.message;
        out << "✗ Failed to lock account: " << account_result.message << std::endl;
    }
}

void accounts_commands::
process_unlock_account(std::ostream& out, nats_session& session,
    std::string account_id) {
    boost::uuids::uuid parsed_id;
    try {
        parsed_id = boost::lexical_cast<boost::uuids::uuid>(account_id);
    } catch (const boost::bad_lexical_cast&) {
        BOOST_LOG_SEV(lg(), error) << "Invalid account ID format: " << account_id;
        out << "✗ Invalid account ID format. Expected UUID." << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating unlock account request for ID: "
                               << account_id;

    iam::messaging::unlock_account_request req;
    req.account_ids = {parsed_id};

    auto result = do_auth_request<iam::messaging::unlock_account_response>(
        out, session, "ores.iam.v1.accounts.unlock", rfl::json::write(req));
    if (!result) return;

    if (result->results.empty()) {
        out << "✗ No results returned from server" << std::endl;
        return;
    }

    const auto& account_result = result->results[0];
    if (account_result.success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully unlocked account: " << account_id;
        out << "✓ Account unlocked successfully!" << std::endl
            << "  Account ID: " << account_id << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Failed to unlock account: " << account_result.message;
        out << "✗ Failed to unlock account: " << account_result.message << std::endl;
    }
}

void accounts_commands::process_create_account(std::ostream& out,
    nats_session& session, std::string principal,
    std::string password, std::string totp_secret, std::string email) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating create account request for principal: "
                               << principal;

    iam::messaging::save_account_request req;
    req.principal = std::move(principal);
    req.password = std::move(password);
    req.totp_secret = std::move(totp_secret);
    req.email = std::move(email);

    auto result = do_auth_request<iam::messaging::save_account_response>(
        out, session, "ores.iam.v1.accounts.save", rfl::json::write(req));
    if (!result) return;

    if (!result->success) {
        BOOST_LOG_SEV(lg(), warn) << "Account creation failed: " << result->message;
        out << "✗ " << result->message << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Successfully created account with ID: "
                              << result->account_id;
    out << "✓ Account created with ID: " << result->account_id << std::endl;
}

void accounts_commands::
process_list_login_info(std::ostream& out, nats_session& session) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating list login info request.";

    auto result = do_auth_request<iam::messaging::list_login_info_response>(
        out, session, "ores.iam.v1.accounts.list-logins",
        rfl::json::write(iam::messaging::list_login_info_request{}));
    if (!result) return;

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->login_infos.size() << " login info records.";
    out << result->login_infos << std::endl;
}

void accounts_commands::
process_logout(std::ostream& out, nats_session& session) {
    if (!session.is_logged_in()) {
        out << "✗ Not logged in." << std::endl;
        return;
    }

    try {
        auto reply = session.authenticated_request("ores.iam.v1.auth.logout",
            rfl::json::write(iam::messaging::logout_request{}));
        auto data_str = std::string(
            reinterpret_cast<const char*>(reply.data.data()), reply.data.size());
        auto result = rfl::json::read<iam::messaging::logout_response>(data_str);
        if (result && result->success) {
            out << "✓ Logged out successfully." << std::endl;
        } else {
            out << "✗ Logout failed." << std::endl;
        }
    } catch (const std::exception& e) {
        out << "✗ Logout failed: " << e.what() << std::endl;
    }
    session.clear_auth();
}

void accounts_commands::
process_bootstrap(std::ostream& out, nats_session& session,
    std::string principal, std::string password, std::string email) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating bootstrap request for principal: "
                               << principal;

    iam::messaging::create_initial_admin_request req;
    req.principal = std::move(principal);
    req.password = std::move(password);
    req.email = std::move(email);

    auto result = do_request<iam::messaging::create_initial_admin_response>(
        out, session, "ores.iam.v1.auth.bootstrap", rfl::json::write(req));
    if (!result) return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Bootstrap successful. Admin account ID: "
                                  << result->account_id
                                  << ", tenant: " << result->tenant_name
                                  << " (" << result->tenant_id << ")";
        out << "✓ Initial admin account created successfully!" << std::endl;
        out << "  Account ID: " << result->account_id << std::endl;
        out << "  Tenant: " << result->tenant_name
            << " (" << result->tenant_id << ")" << std::endl;
        out << "  You can now login with the credentials provided." << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Bootstrap failed: " << result->error_message;
        out << "✗ Bootstrap failed: " << result->error_message << std::endl;
    }
}

void accounts_commands::
process_list_sessions(std::ostream& out, nats_session& session,
    std::string account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating list sessions request.";

    iam::messaging::list_sessions_request req;
    req.limit = 50;

    if (!account_id.empty()) {
        try {
            req.account_id = boost::lexical_cast<boost::uuids::uuid>(account_id);
        } catch (const boost::bad_lexical_cast&) {
            BOOST_LOG_SEV(lg(), error) << "Invalid account ID format: " << account_id;
            out << "✗ Invalid account ID format. Expected UUID." << std::endl;
            return;
        }
    }

    auto result = do_auth_request<iam::messaging::list_sessions_response>(
        out, session, "ores.iam.v1.sessions.list", rfl::json::write(req));
    if (!result) return;

    const auto& sessions = result->sessions;
    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << sessions.size() << " sessions (total: "
                              << result->total_count << ").";

    if (sessions.empty()) {
        out << "No sessions found." << std::endl;
        return;
    }

    out << "Sessions (showing " << sessions.size() << " of "
        << result->total_count << "):" << std::endl;
    out << std::string(80, '-') << std::endl;

    for (const auto& s : sessions) {
        out << "  Start: " << format_time(s.start_time);
        if (s.end_time) {
            out << " - End: " << format_time(*s.end_time);
            if (auto dur = s.duration()) {
                out << " (" << format_duration(*dur) << ")";
            }
        } else {
            out << " [ACTIVE]";
        }
        out << std::endl;
        out << "    IP: " << s.client_ip.to_string();
        if (!s.country_code.empty()) {
            out << " (" << s.country_code << ")";
        }
        out << std::endl;
        if (!s.client_identifier.empty()) {
            out << "    Client: " << s.client_identifier
                << " v" << s.client_version_major << "." << s.client_version_minor;
        }
        if (s.bytes_sent > 0 || s.bytes_received > 0) {
            out << "    Data: sent=" << format_bytes(s.bytes_sent)
                << ", recv=" << format_bytes(s.bytes_received);
        }
        out << std::endl;
    }
}

void accounts_commands::
process_active_sessions(std::ostream& out, nats_session& session) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating active sessions request.";

    auto result = do_auth_request<iam::messaging::get_active_sessions_response>(
        out, session, "ores.iam.v1.sessions.active",
        rfl::json::write(iam::messaging::get_active_sessions_request{}));
    if (!result) return;

    const auto& sessions = result->sessions;
    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << sessions.size() << " active sessions.";

    if (sessions.empty()) {
        out << "No active sessions." << std::endl;
        return;
    }

    out << "Active Sessions (" << sessions.size() << "):" << std::endl;
    out << std::string(80, '-') << std::endl;

    for (const auto& s : sessions) {
        out << "  Started: " << format_time(s.start_time);
        if (auto dur = s.duration()) {
            out << " (running for " << format_duration(*dur) << ")";
        }
        out << std::endl;
        out << "    IP: " << s.client_ip.to_string();
        if (!s.country_code.empty()) {
            out << " (" << s.country_code << ")";
        }
        out << std::endl;
        if (!s.client_identifier.empty()) {
            out << "    Client: " << s.client_identifier
                << " v" << s.client_version_major << "." << s.client_version_minor
                << std::endl;
        }
    }
}

void accounts_commands::
process_session_stats(std::ostream& out, nats_session& session, int days) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating session statistics request for "
                               << days << " days.";

    auto end_date = std::chrono::system_clock::now();
    auto start_time = end_date - std::chrono::hours(24 * days);

    iam::messaging::get_session_statistics_request req;
    req.account_id = boost::uuids::nil_uuid();
    req.start_time = start_time;
    req.end_time = end_date;

    auto result = do_auth_request<iam::messaging::get_session_statistics_response>(
        out, session, "ores.iam.v1.sessions.statistics", rfl::json::write(req));
    if (!result) return;

    const auto& stats = result->statistics;
    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << stats.size() << " daily statistics.";

    if (stats.empty()) {
        out << "No session statistics available for the last "
            << days << " days." << std::endl;
        return;
    }

    out << "Session Statistics (last " << days << " days):" << std::endl;
    out << std::string(80, '-') << std::endl;

    std::uint64_t total_sessions = 0;
    std::uint64_t total_bytes_sent = 0;
    std::uint64_t total_bytes_recv = 0;
    double total_duration = 0;

    for (const auto& s : stats) {
        total_sessions += s.session_count;
        total_bytes_sent += s.total_bytes_sent;
        total_bytes_recv += s.total_bytes_received;
        total_duration += s.avg_duration_seconds * s.session_count;
    }

    double avg_duration = total_sessions > 0 ? total_duration / total_sessions : 0;

    out << "  Total Sessions: " << total_sessions << std::endl;
    out << "  Avg Duration: " << format_duration(std::chrono::seconds(static_cast<int>(avg_duration)))
        << std::endl;
    out << "  Total Data Sent: " << format_bytes(total_bytes_sent) << std::endl;
    out << "  Total Data Received: " << format_bytes(total_bytes_recv) << std::endl;

    if (!stats.empty()) {
        const auto& latest = stats.front();
        if (latest.unique_countries > 0) {
            out << "  Countries (latest): " << latest.unique_countries << std::endl;
        }
    }

    out << std::endl << "Daily breakdown:" << std::endl;
    for (const auto& s : stats) {
        out << "  " << format_time(s.period_start).substr(0, 10)
            << ": " << s.session_count << " sessions";
        if (s.avg_duration_seconds > 0) {
            out << ", avg " << format_duration(std::chrono::seconds(static_cast<int>(s.avg_duration_seconds)));
        }
        out << std::endl;
    }
}

void accounts_commands::
process_get_account_history(std::ostream& out, nats_session& session,
    std::string username) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get account history for: "
                               << username;

    if (!session.is_logged_in()) {
        out << "✗ You must be logged in to get account history." << std::endl;
        return;
    }

    iam::messaging::get_account_history_request req;
    req.username = std::move(username);

    auto result = do_auth_request<iam::messaging::get_account_history_response>(
        out, session, "ores.iam.v1.accounts.history", rfl::json::write(req));
    if (!result) return;

    if (!result->success) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get account history: "
                                  << result->message;
        out << "✗ " << result->message << std::endl;
        return;
    }

    if (result->history.versions.empty()) {
        out << "No history found for this account." << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->history.versions.size()
                              << " history records.";
    out << std::endl << result->history.versions << std::endl;
}

void accounts_commands::
process_account_info(std::ostream& out, nats_session& session,
    std::string username) {
    BOOST_LOG_SEV(lg(), debug) << "Getting comprehensive account info for: "
                               << username;

    if (!session.is_logged_in()) {
        out << "✗ You must be logged in to view account info." << std::endl;
        return;
    }

    // Step 1: Get account details via history request
    iam::messaging::get_account_history_request hist_req;
    hist_req.username = username;

    auto history_result = do_auth_request<iam::messaging::get_account_history_response>(
        out, session, "ores.iam.v1.accounts.history", rfl::json::write(hist_req));
    if (!history_result) return;

    if (!history_result->success) {
        out << "✗ " << history_result->message << std::endl;
        return;
    }

    if (history_result->history.versions.empty()) {
        out << "✗ Account not found: " << username << std::endl;
        return;
    }

    // Get the current version (first in list, most recent)
    const auto& current = history_result->history.versions.front();
    const auto& account = current.data;

    // Display account header
    out << std::endl;
    out << "Account Information: " << username << std::endl;
    out << std::string(50, '=') << std::endl;

    // Display account details
    out << std::endl;
    out << "Details" << std::endl;
    out << "-------" << std::endl;
    out << "  ID:        " << boost::uuids::to_string(account.id) << std::endl;
    out << "  Username:  " << account.username << std::endl;
    out << "  Email:     " << (account.email.empty() ? "(not set)" : account.email) << std::endl;
    out << "  Tenant ID: " << account.tenant_id << std::endl;
    out << "  Type:      " << account.account_type << std::endl;
    out << "  Version:   " << current.version_number << std::endl;
    out << "  Recorded:  " << format_time(current.recorded_at)
        << " by " << current.modified_by << std::endl;

    // Step 2: Get roles for this account
    iam::messaging::get_account_roles_request roles_req;
    roles_req.account_id = account.id;

    out << std::endl;
    out << "Roles" << std::endl;
    out << "-----" << std::endl;

    auto roles_result = do_auth_request<iam::messaging::get_account_roles_response>(
        out, session, "ores.iam.v1.roles.for-account", rfl::json::write(roles_req));
    if (!roles_result) {
        out << "  (failed to retrieve roles)" << std::endl;
    } else if (roles_result->roles.empty()) {
        out << "  (no roles assigned)" << std::endl;
    } else {
        for (const auto& role : roles_result->roles) {
            out << "  - " << role.name;
            if (!role.description.empty()) {
                out << " (" << role.description << ")";
            }
            out << std::endl;
        }
    }

    // Step 3: Get effective permissions for this account
    iam::messaging::get_account_permissions_request perms_req;
    perms_req.account_id = account.id;

    out << std::endl;
    out << "Effective Permissions" << std::endl;
    out << "---------------------" << std::endl;

    auto perms_result = do_auth_request<iam::messaging::get_account_permissions_response>(
        out, session, "ores.iam.v1.permissions.for-account", rfl::json::write(perms_req));
    if (!perms_result) {
        out << "  (failed to retrieve permissions)" << std::endl;
    } else if (perms_result->permission_codes.empty()) {
        out << "  (no permissions)" << std::endl;
    } else {
        // Check for wildcard
        bool has_wildcard = false;
        for (const auto& code : perms_result->permission_codes) {
            if (code == "*") {
                has_wildcard = true;
                break;
            }
        }

        if (has_wildcard) {
            out << "  * (all permissions - superuser)" << std::endl;
        }

        // Group permissions by component
        std::map<std::string, std::vector<std::string>> by_component;
        for (const auto& code : perms_result->permission_codes) {
            if (code == "*") continue;

            auto sep_pos = code.find("::");
            if (sep_pos != std::string::npos) {
                auto component = code.substr(0, sep_pos);
                by_component[component].push_back(code);
            } else {
                by_component["other"].push_back(code);
            }
        }

        for (const auto& [component, codes] : by_component) {
            out << "  [" << component << "]" << std::endl;
            for (const auto& code : codes) {
                out << "    - " << code << std::endl;
            }
        }

        out << std::endl;
        out << "  Total: " << perms_result->permission_codes.size()
            << " permission(s)" << std::endl;
    }

    out << std::endl;
    BOOST_LOG_SEV(lg(), info) << "Successfully displayed account info for: "
                              << username;
}

}
