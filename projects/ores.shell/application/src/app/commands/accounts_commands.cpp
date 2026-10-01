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
#include "ores.shell/app/commands/accounts_commands.hpp"
#include "ores.iam.api/domain/account_table_io.hpp"    // IWYU pragma: keep.
#include "ores.iam.api/domain/login_info_table_io.hpp" // IWYU pragma: keep.
#include "ores.iam.api/messaging/account_protocol.hpp"
#include "ores.iam.api/messaging/authorization_protocol.hpp"
#include "ores.iam.api/messaging/bootstrap_protocol.hpp"
#include "ores.iam.api/messaging/login_info_protocol.hpp"
#include "ores.iam.api/messaging/login_protocol.hpp"
#include "ores.iam.api/messaging/session_operations_protocol.hpp"
#include "ores.iam.api/messaging/session_protocol.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.api/messaging/party_protocol.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include <algorithm>
#include "ores.shell/app/commands/history_diff_renderer.hpp"
#include "ores.shell/app/commands/rbac_commands.hpp"
#include "ores.shell/app/login_helpers.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.shell/app/shell_root_menu.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <cli/cli.h>
#include <functional>
#include <iomanip>
#include <map>
#include <ostream>
#include <sstream>
#include <vector>

namespace ores::shell::app::commands {

using namespace ores::logging;
using ores::nats::service::nats_client;

namespace {

std::string format_time(std::chrono::system_clock::time_point tp) {
    return ores::platform::time::datetime::to_local_display_string(tp);
}

} // anonymous namespace

void accounts_commands::register_commands(cli::Menu& root_menu,
                                          nats_client& session,
                                          pagination_context& pagination) {
    // The generated account unit owns the accounts menu. Every verb below is
    // one the model cannot express: signing this client in and out, the
    // generic history renderer, a username-addressed info view, and the
    // default party. The account, login-info, session and account-operation
    // reads are generated units under their own menus.
    ores::shell::app::extend_menu(
        root_menu, "accounts", [&session, &pagination](cli::Menu& accounts_menu) {
            // Register list callback for navigation
            pagination.register_list_callback("accounts",
                                              [&session, &pagination](std::ostream& out) {
                                                  process_list_accounts(out, session, pagination);
                                              });

            accounts_menu.Insert(
                "login",
                [&session](std::ostream& out, std::string principal, std::string password) {
                    process_login(std::ref(out),
                                  std::ref(session),
                                  std::move(principal),
                                  std::move(password));
                },
                "Login with principal (username@hostname or username) and password");

            accounts_menu.Insert(
                "logout",
                [&session](std::ostream& out) { process_logout(std::ref(out), std::ref(session)); },
                "Logout the current user");

            accounts_menu.Insert(
                "history",
                [&session](std::ostream& out, std::string username) {
                    process_get_account_history(
                        std::ref(out), std::ref(session), std::move(username));
                },
                "Get version history for an account by username");

            accounts_menu.Insert(
                "info",
                [&session](std::ostream& out, std::string username) {
                    process_account_info(std::ref(out), std::ref(session), std::move(username));
                },
                "Show comprehensive account info (username) - details, roles, permissions");

            accounts_menu.Insert(
                "set-default-party",
                [&session](std::ostream& out, std::string party_ref) {
                    process_set_default_party(
                        std::ref(out), std::ref(session), std::move(party_ref));
                },
                "Set the logged-in account's default party for quick-login "
                "(party-uuid-or-full-name)");
        });

    // Top-level aliases, so a caller need not enter the menu first.
    ores::shell::app::claim_name(root_menu, "login");
    root_menu.Insert(
        "login",
        [&session](std::ostream& out, std::string principal, std::string password) {
            process_login(
                std::ref(out), std::ref(session), std::move(principal), std::move(password));
        },
        "Login with principal (username@hostname or username) and password");

    ores::shell::app::claim_name(root_menu, "logout");
    root_menu.Insert(
        "logout",
        [&session](std::ostream& out) { process_logout(std::ref(out), std::ref(session)); },
        "Logout the current user (alias for 'accounts logout')");
}

void accounts_commands::process_list_accounts(std::ostream& out,
                                              nats_client& session,
                                              pagination_context& pagination) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating list account request.";

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to list accounts." << std::endl;
        return;
    }

    auto& state = pagination.state_for("accounts");

    // The derived list is the entity's own read now, so the page and the total
    // are the protocol's rather than a hand-written request's.
    iam::messaging::list_accounts_request req;
    req.offset = state.current_offset;
    req.limit = pagination.page_size();

    auto result = do_auth_request<iam::messaging::list_accounts_response>(
        out, session, std::string(req.nats_subject), req);
    if (!result)
        return;

    state.total_count = result->total;
    pagination.set_last_entity("accounts");

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << result->accounts.size()
                              << " accounts.";
    out << result->accounts << std::endl;

    // Display pagination info
    const auto page = (state.current_offset / pagination.page_size()) + 1;
    const auto total_pages =
        state.total_count > 0 ?
            ((state.total_count + pagination.page_size() - 1) / pagination.page_size()) :
            1;
    out << "\nPage " << page << " of " << total_pages << " (" << result->accounts.size() << " of "
        << state.total_count << " total)" << std::endl;
}

void accounts_commands::process_login(std::ostream& out,
                                      nats_client& session,
                                      std::string principal,
                                      std::string password) {
    iam::messaging::login_request req;
    req.principal = std::move(principal);
    req.password = std::move(password);

    auto result = do_request<iam::messaging::login_response>(
        out, session, iam::messaging::login_request::nats_subject, req);
    if (!result)
        return;

    if (!result->success) {
        fail(out) << "Login failed: " << result->message << std::endl;
        return;
    }

    auto selected = complete_login(out, session, *result);
    if (!selected)
        return;

    out << "✓ Login successful!" << std::endl;
    out << "  User: " << result->username << std::endl;
    out << "  Tenant: " << result->tenant_name << " (" << result->tenant_id << ")" << std::endl;
    if (!selected->empty())
        out << "  Party: " << *selected << std::endl;
}

void accounts_commands::process_logout(std::ostream& out, nats_client& session) {
    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    try {
        auto result = do_auth_request<iam::messaging::logout_response>(
            out,
            session,
            iam::messaging::logout_request::nats_subject,
            iam::messaging::logout_request{});
        if (result && result->success) {
            out << "✓ Logged out successfully." << std::endl;
        } else {
            fail(out) << "Logout failed." << std::endl;
        }
    } catch (const std::exception& e) {
        fail(out) << "Logout failed: " << e.what() << std::endl;
    }
    session.clear_auth();
}

void accounts_commands::process_get_account_history(std::ostream& out,
                                                    nats_client& session,
                                                    std::string username) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get account history for: " << username;
    /*
     * One generic history request serves every entity, so an account's history
     * is read the way any other entity's is. The account's own hand-shaped
     * history protocol is gone: it was keyed by the very username this command
     * takes, which is the account's declared key, so the generic path covers it
     * exactly.
     */
    render_history_diff(out, session, "ores.iam.account", std::move(username), std::nullopt);
}

void accounts_commands::process_set_default_party(std::ostream& out,
                                                  nats_client& session,
                                                  std::string party_ref) {
    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    refdata::messaging::list_parties_request parties_req;
    parties_req.limit = 1000;
    auto parties = do_auth_request<refdata::messaging::list_parties_response>(
        out, session, refdata::messaging::list_parties_request::nats_subject, parties_req);
    if (!parties || parties->result.outcome != ores::utility::domain::outcome::ok)
        return;

    std::optional<boost::uuids::uuid> ref_uuid;
    try {
        ref_uuid = boost::lexical_cast<boost::uuids::uuid>(party_ref);
    } catch (const boost::bad_lexical_cast&) {
    }

    std::optional<refdata::domain::party> party;
    for (const auto& p : parties->parties) {
        if ((ref_uuid && p.id == *ref_uuid) || (!ref_uuid && p.full_name == party_ref)) {
            party = p;
            break;
        }
    }
    if (!party) {
        fail(out) << "Party not found (by UUID or exact full name): " << party_ref << std::endl;
        return;
    }

    iam::messaging::set_my_default_party_request req;
    req.party_id = boost::uuids::to_string(party->id);

    auto result = do_auth_request<iam::messaging::set_my_default_party_response>(
        out, session, iam::messaging::set_my_default_party_request::nats_subject, req);
    if (!result)
        return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Default party set to: " << party->full_name;
        out << "✓ Default party set to '" << party->full_name << "'." << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Failed to set default party: " << result->message;
        fail(out) << "Failed to set default party: " << result->message << std::endl;
    }
}

void accounts_commands::process_account_info(std::ostream& out,
                                             nats_client& session,
                                             std::string username) {
    BOOST_LOG_SEV(lg(), debug) << "Getting comprehensive account info for: " << username;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to view account info." << std::endl;
        return;
    }

    /*
     * Step 1: the account itself. This asked for the history and read the
     * record out of the newest version, which is the same record the single
     * -record read answers with -- and answers with directly, rather than
     * making the caller fetch every version to look at one.
     */
    iam::messaging::get_account_request get_req;
    get_req.key.username = username;

    auto account_result = do_auth_request<iam::messaging::get_account_response>(
        out, session, std::string(get_req.nats_subject), get_req);
    if (!account_result)
        return;

    if (!account_result->account) {
        fail(out) << "Account not found: " << username << std::endl;
        return;
    }

    const auto& account = *account_result->account;

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
    out << "  Version:   " << account.version << std::endl;
    out << "  Recorded:  " << format_time(account.recorded_at) << " by " << account.modified_by
        << std::endl;

    // Step 2: Get the account's roles, each with its permissions and assignment tail
    iam::messaging::get_account_roles_request roles_req;
    roles_req.account_id = boost::uuids::to_string(account.id);

    out << std::endl;
    out << "Roles" << std::endl;
    out << "-----" << std::endl;

    std::vector<std::string> permission_codes;
    auto roles_result = do_auth_request<iam::messaging::get_account_roles_response>(
        out, session, iam::messaging::get_account_roles_request::nats_subject, roles_req);
    if (!roles_result) {
        out << "  (failed to retrieve roles)" << std::endl;
    } else if (roles_result->result.outcome != ores::utility::domain::outcome::ok) {
        out << "  (" << roles_result->result.message << ")" << std::endl;
    } else if (roles_result->roles.empty()) {
        out << "  (no roles assigned)" << std::endl;
    } else {
        for (const auto& entry : roles_result->roles) {
            out << "  - " << entry.role.name;
            if (!entry.role.description.empty()) {
                out << " (" << entry.role.description << ")";
            }
            out << std::endl;
            permission_codes.insert(permission_codes.end(),
                                    entry.permission_codes.begin(),
                                    entry.permission_codes.end());
        }
        /*
         * Two roles may grant the same permission, and the count below is of
         * distinct permissions rather than of grants, so the codes are
         * deduplicated before anything reads them.
         */
        std::sort(permission_codes.begin(), permission_codes.end());
        permission_codes.erase(std::unique(permission_codes.begin(), permission_codes.end()),
                               permission_codes.end());
    }

    // Step 3: Show the effective permissions the roles above carry
    out << std::endl;
    out << "Effective Permissions" << std::endl;
    out << "---------------------" << std::endl;

    if (permission_codes.empty()) {
        out << "  (no permissions)" << std::endl;
    } else {
        // Check for wildcard
        bool has_wildcard = false;
        for (const auto& code : permission_codes) {
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
        for (const auto& code : permission_codes) {
            if (code == "*")
                continue;

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
        out << "  Total: " << permission_codes.size() << " permission(s)" << std::endl;
    }

    out << std::endl;
    BOOST_LOG_SEV(lg(), info) << "Successfully displayed account info for: " << username;
}

}
