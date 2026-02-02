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
#include "ores.comms.shell/app/commands/accounts_commands.hpp"

#include <iomanip>
#include <ostream>
#include <sstream>
#include <functional>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <cli/cli.h>
#include "ores.platform/time/datetime.hpp"
#include "ores.iam/messaging/protocol.hpp"
#include "ores.iam/messaging/bootstrap_protocol.hpp"
#include "ores.iam/messaging/session_protocol.hpp"
#include "ores.iam/messaging/account_history_protocol.hpp"
#include "ores.iam/domain/account_table_io.hpp"  // IWYU pragma: keep.
#include "ores.iam/domain/login_info_table_io.hpp"  // IWYU pragma: keep.
#include "ores.iam/domain/account_version_table_io.hpp"  // IWYU pragma: keep.
#include "ores.comms.shell/app/commands/rbac_commands.hpp"

namespace ores::comms::shell::app::commands {

using namespace ores::logging;
using comms::messaging::message_type;
using comms::net::client_session;
using comms::net::client_session_error;

void accounts_commands::
register_commands(cli::Menu& root_menu, client_session& session) {
    auto accounts_menu =
        std::make_unique<cli::Menu>("accounts");

    accounts_menu->Insert("create", [&session](std::ostream & out,
            std::string username, std::string password, std::string totp_secret,
            std::string email) {
        process_create_account(std::ref(out),
            std::ref(session), std::move(username),
            std::move(password), std::move(totp_secret),
                std::move(email));
    }, "Create a new account (username password totp_secret email)");

    accounts_menu->Insert("list", [&session](std::ostream& out) {
        process_list_accounts(std::ref(out), std::ref(session));
    }, "Retrieve all accounts from the server");

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
    }, "Assign a role to an account (account_id role_id)");

    accounts_menu->Insert("revoke-role", [&session](std::ostream& out,
            std::string account_id, std::string role_id) {
        rbac_commands::process_revoke_role(std::ref(out), std::ref(session),
            std::move(account_id), std::move(role_id));
    }, "Revoke a role from an account (account_id role_id)");

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
process_list_accounts(std::ostream& out, client_session& session) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating list account request.";

    using iam::messaging::get_accounts_request;
    using iam::messaging::get_accounts_response;
    auto result = session.process_authenticated_request<get_accounts_request,
                                                        get_accounts_response,
                                                        message_type::get_accounts_request>
        (get_accounts_request{});

    if (!result) {
        out << "✗ " << to_string(result.error()) << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->accounts.size() << " accounts.";
    out << result->accounts << std::endl;
}

void accounts_commands::
process_login(std::ostream& out, client_session& session,
    std::string principal, std::string password) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating login request for principal: "
                               << principal;

    using iam::messaging::login_request;
    auto result = session.process_request(
        login_request{
            .principal = principal,
            .password = std::move(password)
        });

    if (!result) {
        out << "✗ " << to_string(result.error()) << std::endl;
        return;
    }

    const auto& response = *result;
    if (!response.success) {
        BOOST_LOG_SEV(lg(), warn) << "Login failed: " << response.error_message;
        out << "✗ Login failed: " << response.error_message << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Login successful for user: " << response.username;
    out << "✓ Login successful!" << std::endl;

    // Update session state
    // Note: Permission checks are now handled server-side via RBAC
    comms::net::client_session_info info{
        .account_id = response.account_id,
        .username = response.username
    };
    session.set_session_info(std::move(info));
}

void accounts_commands::
process_lock_account(std::ostream& out, client_session& session,
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

    using iam::messaging::lock_account_request;
    using iam::messaging::lock_account_response;
    auto result = session.process_authenticated_request<lock_account_request,
                                                        lock_account_response,
                                                        message_type::lock_account_request>
        (lock_account_request{.account_ids = {parsed_id}});

    if (!result) {
        out << "✗ " << comms::net::to_string(result.error()) << std::endl;
        return;
    }

    const auto& response = *result;
    if (response.results.empty()) {
        out << "✗ No results returned from server" << std::endl;
        return;
    }

    const auto& account_result = response.results[0];
    if (account_result.success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully locked account: "
                                  << account_id;
        out << "✓ Account locked successfully!" << std::endl
            << "  Account ID: " << account_id << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn)
            << "Failed to lock account: " << account_result.message;
        out << "✗ Failed to lock account: " << account_result.message << std::endl;
    }
}

void accounts_commands::
process_unlock_account(std::ostream& out, client_session& session,
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

    using iam::messaging::unlock_account_request;
    using iam::messaging::unlock_account_response;
    auto result = session.process_authenticated_request<unlock_account_request,
                                                        unlock_account_response,
                                                        message_type::unlock_account_request>
        (unlock_account_request{.account_ids = {parsed_id}});

    if (!result) {
        out << "✗ " << comms::net::to_string(result.error()) << std::endl;
        return;
    }

    const auto& response = *result;
    if (response.results.empty()) {
        out << "✗ No results returned from server" << std::endl;
        return;
    }

    const auto& account_result = response.results[0];
    if (account_result.success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully unlocked account: "
                                  << account_id;
        out << "✓ Account unlocked successfully!" << std::endl
            << "  Account ID: " << account_id << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn)
            << "Failed to unlock account: " << account_result.message;
        out << "✗ Failed to unlock account: " << account_result.message << std::endl;
    }
}

void accounts_commands::process_create_account(std::ostream& out,
    client_session& session, std::string username,
    std::string password, std::string totp_secret, std::string email) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating create account request.";

    using iam::messaging::save_account_request;
    using iam::messaging::save_account_response;
    auto result = session.process_authenticated_request<save_account_request,
                                                        save_account_response,
                                                        message_type::save_account_request>
        (save_account_request {
            .username = std::move(username),
            .password = std::move(password),
            .totp_secret = std::move(totp_secret),
            .email = std::move(email)
        });

    if (!result) {
        out << "✗ " << comms::net::to_string(result.error()) << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Successfully created account with ID: "
                              << result->account_id;
    out << "✓ Account created with ID: " << result->account_id << std::endl;
}

void accounts_commands::
process_list_login_info(std::ostream& out, client_session& session) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating list login info request.";

    using iam::messaging::list_login_info_request;
    using iam::messaging::list_login_info_response;
    auto result = session.process_authenticated_request<list_login_info_request,
                                                        list_login_info_response,
                                                        message_type::list_login_info_request>
        (list_login_info_request{});

    if (!result) {
        out << "✗ " << to_string(result.error()) << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->login_infos.size() << " login info records.";
    out << result->login_infos << std::endl;
}

void accounts_commands::
process_logout(std::ostream& out, client_session& session) {
    BOOST_LOG_SEV(lg(), debug) << "Processing logout request.";

    // Check if logged in
    if (!session.is_logged_in()) {
        out << "✗ Not logged in." << std::endl;
        return;
    }

    using iam::messaging::logout_request;
    using iam::messaging::logout_response;
    auto result = session.process_authenticated_request<logout_request,
                                                        logout_response,
                                                        message_type::logout_request>
        (logout_request{});

    if (!result) {
        out << "✗ " << to_string(result.error()) << std::endl;
        return;
    }

    const auto& response = *result;
    if (response.success) {
        BOOST_LOG_SEV(lg(), info) << "Logout successful.";
        out << "✓ Logged out successfully." << std::endl;
        session.clear_session_info();
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Logout failed: " << response.message;
        out << "✗ Logout failed: " << response.message << std::endl;
    }
}

void accounts_commands::
process_bootstrap(std::ostream& out, client_session& session,
    std::string principal, std::string password, std::string email) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating bootstrap request for principal: "
                               << principal;

    using iam::messaging::create_initial_admin_request;
    auto result = session.process_request(create_initial_admin_request{
        .principal = std::move(principal),
        .password = std::move(password),
        .email = std::move(email)
    });

    if (!result) {
        out << "✗ " << to_string(result.error()) << std::endl;
        return;
    }

    const auto& response = *result;
    if (response.success) {
        BOOST_LOG_SEV(lg(), info) << "Bootstrap successful. Admin account ID: "
                                  << response.account_id;
        out << "✓ Initial admin account created successfully!" << std::endl;
        out << "  Account ID: " << response.account_id << std::endl;
        out << "  You can now login with the credentials provided." << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Bootstrap failed: " << response.error_message;
        out << "✗ Bootstrap failed: " << response.error_message << std::endl;
    }
}

namespace {

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

std::string format_time(std::chrono::system_clock::time_point tp) {
    return ores::platform::time::datetime::format_time_point(tp);
}

}

void accounts_commands::
process_list_sessions(std::ostream& out, client_session& session,
    std::string account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating list sessions request.";

    boost::uuids::uuid parsed_id{};
    if (!account_id.empty()) {
        try {
            parsed_id = boost::lexical_cast<boost::uuids::uuid>(account_id);
        } catch (const boost::bad_lexical_cast&) {
            BOOST_LOG_SEV(lg(), error) << "Invalid account ID format: " << account_id;
            out << "✗ Invalid account ID format. Expected UUID." << std::endl;
            return;
        }
    }

    using iam::messaging::list_sessions_request;
    using iam::messaging::list_sessions_response;
    auto result = session.process_authenticated_request<list_sessions_request,
                                                        list_sessions_response,
                                                        message_type::list_sessions_request>
        (list_sessions_request{.account_id = parsed_id, .limit = 50});

    if (!result) {
        out << "✗ " << to_string(result.error()) << std::endl;
        return;
    }

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
process_active_sessions(std::ostream& out, client_session& session) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating active sessions request.";

    using iam::messaging::get_active_sessions_request;
    using iam::messaging::get_active_sessions_response;
    auto result = session.process_authenticated_request<get_active_sessions_request,
                                                        get_active_sessions_response,
                                                        message_type::get_active_sessions_request>
        (get_active_sessions_request{});

    if (!result) {
        out << "✗ " << to_string(result.error()) << std::endl;
        return;
    }

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
process_session_stats(std::ostream& out, client_session& session, int days) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating session statistics request for "
                               << days << " days.";

    auto end_date = std::chrono::system_clock::now();
    auto start_time = end_date - std::chrono::hours(24 * days);

    using iam::messaging::get_session_statistics_request;
    using iam::messaging::get_session_statistics_response;
    auto result = session.process_authenticated_request<get_session_statistics_request,
                                                        get_session_statistics_response,
                                                        message_type::get_session_statistics_request>
        (get_session_statistics_request{
            .account_id = boost::uuids::nil_uuid(),
            .start_time = start_time,
            .end_time = end_date
        });

    if (!result) {
        out << "✗ " << comms::net::to_string(result.error()) << std::endl;
        return;
    }

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
process_get_account_history(std::ostream& out, client_session& session,
    std::string username) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get account history for: "
                               << username;

    // Check if logged in
    if (!session.session_info()) {
        out << "✗ You must be logged in to get account history." << std::endl;
        return;
    }

    using iam::messaging::get_account_history_request;
    using iam::messaging::get_account_history_response;
    auto result = session.process_authenticated_request<get_account_history_request,
                                                        get_account_history_response,
                                                        message_type::get_account_history_request>
        (get_account_history_request{.username = std::move(username)});

    if (!result) {
        out << "✗ " << comms::net::to_string(result.error()) << std::endl;
        return;
    }

    const auto& response = *result;
    if (!response.success) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get account history: "
                                  << response.message;
        out << "✗ " << response.message << std::endl;
        return;
    }

    if (response.history.versions.empty()) {
        out << "No history found for this account." << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << response.history.versions.size()
                              << " history records.";
    out << response.history.versions << std::endl;
}

}
