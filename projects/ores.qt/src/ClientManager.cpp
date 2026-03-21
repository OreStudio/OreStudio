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
#include "ores.qt/ClientManager.hpp"

#include <sstream>
#include <QDateTime>
#include <QTimeZone>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/string_generator.hpp>
#include "ores.nats/config/nats_options.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.eventing/domain/entity_change_event.hpp"
#include "ores.iam/messaging/login_protocol.hpp"
#include "ores.iam/messaging/signup_protocol.hpp"
#include "ores.iam/messaging/bootstrap_protocol.hpp"
#include "ores.iam/messaging/session_protocol.hpp"
#include "ores.iam/messaging/session_samples_protocol.hpp"
#include "ores.iam/messaging/account_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

ClientManager::ClientManager(std::shared_ptr<eventing::service::event_bus> event_bus,
                             QObject* parent)
    : QObject(parent), event_bus_(std::move(event_bus)) {
    BOOST_LOG_SEV(lg(), debug) << "ClientManager created";
    refresh_timer_ = new QTimer(this);
    refresh_timer_->setSingleShot(true);
    QObject::connect(refresh_timer_, &QTimer::timeout, this, &ClientManager::onRefreshTimer);
}

ClientManager::~ClientManager() {
    BOOST_LOG_SEV(lg(), debug) << "ClientManager destroyed";
    disconnect();
}

LoginResult ClientManager::connect(const std::string& host, std::uint16_t port) {
    BOOST_LOG_SEV(lg(), info) << "Connecting to " << host << ":" << port
                              << " (namespace: '"
                              << (subject_prefix_.empty() ? "(none)" : subject_prefix_)
                              << "')";

    // If already connected, disconnect first
    if (session_.is_connected()) {
        disconnect();
    }

    try {
        nats::config::nats_options opts;
        opts.url = "nats://" + host + ":" + std::to_string(port);
        opts.subject_prefix = subject_prefix_;

        session_.connect(std::move(opts));

        // Check bootstrap status
        BOOST_LOG_SEV(lg(), debug) << "Checking bootstrap status...";
        auto bootstrap_result = process_request(
            iam::messaging::bootstrap_status_request{});
        if (bootstrap_result && bootstrap_result->is_in_bootstrap_mode) {
            BOOST_LOG_SEV(lg(), info) << "System is in bootstrap mode";
            connected_host_ = host;
            connected_port_ = port;
            QMetaObject::invokeMethod(this, "connected", Qt::QueuedConnection);
            return {
                .success = false,
                .error_message = QString::fromStdString(bootstrap_result->message),
                .bootstrap_mode = true
            };
        }

        connected_host_ = host;
        connected_port_ = port;
        QMetaObject::invokeMethod(this, "connected", Qt::QueuedConnection);
        return {.success = true, .error_message = {}};

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Connection failed: " << e.what();
        return {.success = false, .error_message = QString::fromStdString(e.what())};
    }
}

LoginResult ClientManager::login(const std::string& username,
                                 const std::string& password) {
    BOOST_LOG_SEV(lg(), info) << "Logging in as " << username;

    if (!session_.is_connected()) {
        return {.success = false, .error_message = "Not connected to server"};
    }

    try {
        iam::messaging::login_request request{
            .principal = username,
            .password = password
        };

        auto result = process_request(std::move(request));
        if (!result) {
            BOOST_LOG_SEV(lg(), error) << "LOGIN FAILURE: " << result.error();
            return {.success = false,
                    .error_message = QString::fromStdString(result.error())};
        }

        const auto& response = *result;
        if (!response.success) {
            BOOST_LOG_SEV(lg(), warn) << "LOGIN FAILURE: Server rejected login for '"
                                      << username << "': " << response.error_message;
            return {.success = false,
                    .error_message = QString::fromStdString(response.error_message)};
        }

        // An empty token means the IAM service has no JWT signing key configured.
        if (response.token.empty()) {
            BOOST_LOG_SEV(lg(), error) << "LOGIN FAILURE: IAM service returned "
                                       << "empty JWT token - signing key not configured";
            return {.success = false,
                    .error_message =
                        "IAM service is not configured for JWT signing.\n\n"
                        "Run generate_keys.sh in publish/bin/ to generate "
                        "the private key, then restart the IAM service."};
        }

        // Store auth in session
        session_.set_auth(shell::service::nats_session::login_info{
            .jwt = response.token,
            .username = response.username,
            .tenant_id = response.tenant_id,
            .tenant_name = response.tenant_name
        });

        // Store local state
        stored_username_ = username;
        stored_password_ = password;
        current_email_ = response.email;
        if (!response.account_id.empty()) {
            try {
                boost::uuids::string_generator gen;
                current_account_id_ = gen(response.account_id);
            } catch (const std::exception&) {
                // ignore bad UUID
            }
        }

        // Parse selected party UUID (comes as string from JSON)
        boost::uuids::uuid selected_party_id{};
        if (!response.selected_party_id.empty()) {
            try {
                boost::uuids::string_generator gen;
                selected_party_id = gen(response.selected_party_id);
            } catch (const std::exception&) {
                // ignore bad UUID
            }
        }

        // Set party context
        if (!selected_party_id.is_nil()) {
            current_party_id_ = selected_party_id;
            if (!response.available_parties.empty()) {
                current_party_name_ = QString::fromStdString(
                    response.available_parties.front().name);
                current_party_category_ = QString::fromStdString(
                    response.available_parties.front().party_category);
            }
        } else {
            current_party_id_ = {};
            current_party_name_.clear();
            current_party_category_.clear();
        }

        BOOST_LOG_SEV(lg(), info) << "LOGIN SUCCESS: User '" << response.username
                                  << "' authenticated";

        // Build available parties list
        std::vector<PartyInfo> available_parties;
        available_parties.reserve(response.available_parties.size());
        for (const auto& ps : response.available_parties) {
            boost::uuids::uuid party_uuid{};
            if (!ps.id.empty()) {
                try {
                    boost::uuids::string_generator gen;
                    party_uuid = gen(ps.id);
                } catch (const std::exception&) {}
            }
            available_parties.push_back(PartyInfo{
                .id = party_uuid,
                .name = QString::fromStdString(ps.name)
            });
        }

        arm_refresh_timer(response.access_lifetime_s);
        emit loggedIn();
        return {
            .success = true,
            .error_message = {},
            .password_reset_required = response.password_reset_required,
            .tenant_bootstrap_mode = response.tenant_bootstrap_mode,
            .selected_party_id = selected_party_id,
            .available_parties = std::move(available_parties)
        };

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Login failed: " << e.what();
        return {.success = false, .error_message = QString::fromStdString(e.what())};
    }
}

LoginResult ClientManager::connectAndLogin(
    const std::string& host,
    std::uint16_t port,
    const std::string& username,
    const std::string& password) {

    auto connect_result = connect(host, port);
    if (connect_result.bootstrap_mode || !connect_result.success) {
        return connect_result;
    }
    return login(username, password);
}

LoginResult ClientManager::testConnection(
    const std::string& host,
    std::uint16_t port,
    const std::string& username,
    const std::string& password) {

    BOOST_LOG_SEV(lg(), info) << "Testing connection to " << host << ":" << port;

    try {
        shell::service::nats_session temp_session;
        nats::config::nats_options opts;
        opts.url = "nats://" + host + ":" + std::to_string(port);
        opts.subject_prefix = subject_prefix_;
        temp_session.connect(std::move(opts));

        // Attempt login
        iam::messaging::login_request request{
            .principal = username,
            .password = password
        };
        const auto json_body = rfl::json::write(request);
        auto msg = temp_session.request(
            iam::messaging::login_request::nats_subject, json_body);
        temp_session.disconnect();

        const std::string_view data(
            reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
        auto resp = rfl::json::read<iam::messaging::login_response>(data);
        if (!resp || !resp->success) {
            const std::string err = resp ? resp->error_message : "Invalid response";
            return {.success = false, .error_message = QString::fromStdString(err)};
        }
        return {.success = true, .error_message = {}};

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Test connection failed: " << e.what();
        return {.success = false, .error_message = QString::fromStdString(e.what())};
    }
}

SignupResult ClientManager::signup(
    const std::string& host,
    std::uint16_t port,
    const std::string& username,
    const std::string& email,
    const std::string& password) {

    BOOST_LOG_SEV(lg(), info) << "Attempting signup to " << host << ":" << port;

    try {
        shell::service::nats_session temp_session;
        nats::config::nats_options opts;
        opts.url = "nats://" + host + ":" + std::to_string(port);
        opts.subject_prefix = subject_prefix_;
        temp_session.connect(std::move(opts));

        iam::messaging::signup_request request{
            .principal = username,
            .password = password,
            .email = email
        };
        const auto json_body = rfl::json::write(request);
        auto msg = temp_session.request(
            iam::messaging::signup_request::nats_subject, json_body);
        temp_session.disconnect();

        const std::string_view data(
            reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
        auto resp = rfl::json::read<iam::messaging::signup_response>(data);
        if (!resp || !resp->success) {
            const std::string err = resp ? resp->message : "Invalid response";
            return {.success = false,
                    .error_message = QString::fromStdString(err)};
        }
        return {.success = true,
                .error_message = {},
                .username = QString::fromStdString(username)};

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Signup failed: " << e.what();
        return {.success = false,
                .error_message = QString::fromStdString(e.what())};
    }
}

void ClientManager::subscribeToEvent(const std::string& subject) {
    if (nats_subscriptions_.count(subject))
        return;

    auto cl = session_.get_client();
    if (!cl) {
        BOOST_LOG_SEV(lg(), warn) << "subscribeToEvent: not connected, skipping '"
                                  << subject << "'";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Subscribing to NATS event: " << subject;
    try {
        auto sub = cl->subscribe(subject,
            [this, subject](nats::message msg) {
                try {
                    const std::string_view json(
                        reinterpret_cast<const char*>(msg.data.data()),
                        msg.data.size());
                    auto result =
                        rfl::json::read<eventing::domain::entity_change_event>(json);
                    if (!result) {
                        BOOST_LOG_SEV(lg(), warn)
                            << "Failed to parse event on '" << subject << "': "
                            << result.error().what();
                        return;
                    }
                    const auto& ev = *result;
                    const auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(
                        ev.timestamp.time_since_epoch()).count();
                    QDateTime ts = QDateTime::fromMSecsSinceEpoch(ms, QTimeZone::UTC);
                    QStringList ids;
                    ids.reserve(static_cast<int>(ev.entity_ids.size()));
                    for (const auto& id : ev.entity_ids)
                        ids.append(QString::fromStdString(id));
                    QString eventType = QString::fromStdString(ev.entity);
                    QString tenantId  = QString::fromStdString(ev.tenant_id);
                    QMetaObject::invokeMethod(this,
                        [this, eventType, ts, ids, tenantId]() {
                            emit notificationReceived(
                                eventType, ts, ids, tenantId, 0, {});
                        }, Qt::QueuedConnection);
                } catch (const std::exception& e) {
                    BOOST_LOG_SEV(lg(), error)
                        << "Exception in event handler for '" << subject
                        << "': " << e.what();
                }
            });
        nats_subscriptions_.emplace(subject, std::move(sub));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to subscribe to '" << subject
                                   << "': " << e.what();
    }
}

void ClientManager::unsubscribeFromEvent(const std::string& subject) {
    if (nats_subscriptions_.erase(subject) > 0) {
        BOOST_LOG_SEV(lg(), debug) << "Unsubscribed from event: " << subject;
    }
}

void ClientManager::disconnect() {
    BOOST_LOG_SEV(lg(), info) << "Disconnecting";
    if (refresh_timer_)
        refresh_timer_->stop();
    nats_subscriptions_.clear();
    if (session_.is_logged_in()) {
        logout();
    }
    if (session_.is_connected()) {
        session_.disconnect();
    }
    session_.clear_auth();
    connected_host_.clear();
    connected_port_ = 0;
    current_account_id_.reset();
    current_email_.clear();
    current_party_id_ = {};
    current_party_name_.clear();
    current_party_category_.clear();
    stored_username_.clear();
    stored_password_.clear();
    disconnected_since_ = std::chrono::steady_clock::now();
    QMetaObject::invokeMethod(this, "disconnected", Qt::QueuedConnection);
}

bool ClientManager::logout() {
    BOOST_LOG_SEV(lg(), info) << "Logging out";
    if (!session_.is_logged_in()) {
        return false;
    }

    try {
        auto result = process_authenticated_request(iam::messaging::logout_request{});
        session_.clear_auth();
        return result && result->success;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Logout failed: " << e.what();
        session_.clear_auth();
        return false;
    }
}

bool ClientManager::isConnected() const {
    return session_.is_connected();
}

bool ClientManager::selectParty(const boost::uuids::uuid& party_id,
    const QString& party_name) {
    BOOST_LOG_SEV(lg(), info) << "Selecting party: " << party_name.toStdString();

    try {
        iam::messaging::select_party_request request{
            .party_id = boost::uuids::to_string(party_id)};
        auto result = process_authenticated_request(std::move(request));

        if (!result || !result->success) {
            BOOST_LOG_SEV(lg(), warn) << "selectParty: server rejected party selection";
            return false;
        }

        // Update JWT with new token from select_party response
        if (!result->token.empty()) {
            auto auth = session_.auth();
            shell::service::nats_session::login_info updated_auth = auth;
            updated_auth.jwt = result->token;
            if (!result->party_name.empty())
                updated_auth.tenant_name = result->party_name;
            session_.set_auth(updated_auth);
        }

        current_party_id_ = party_id;
        current_party_name_ = party_name;
        arm_refresh_timer(result->access_lifetime_s);
        BOOST_LOG_SEV(lg(), info) << "Party selected: " << party_name.toStdString();
        return true;

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "selectParty failed: " << e.what();
        return false;
    }
}

std::optional<SessionListResult> ClientManager::listSessions(
    const boost::uuids::uuid& accountId,
    std::uint32_t limit,
    std::uint32_t offset) {

    try {
        iam::messaging::list_sessions_request request{
            .account_id = boost::uuids::to_string(accountId),
            .limit = static_cast<int>(limit),
            .offset = static_cast<int>(offset)
        };

        auto result = process_authenticated_request(std::move(request));
        if (!result) {
            BOOST_LOG_SEV(lg(), error) << "listSessions failed: " << result.error();
            return std::nullopt;
        }

        return SessionListResult{
            .sessions = std::move(result->sessions),
            .total_count = static_cast<std::uint32_t>(result->total_count)
        };
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "listSessions failed: " << e.what();
        return std::nullopt;
    }
}

std::optional<std::vector<iam::domain::session>>
ClientManager::getActiveSessions() {
    try {
        iam::messaging::get_active_sessions_request request;
        auto result = process_authenticated_request(std::move(request));
        if (!result) {
            BOOST_LOG_SEV(lg(), error) << "getActiveSessions failed: " << result.error();
            return std::nullopt;
        }
        return std::move(result->sessions);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "getActiveSessions failed: " << e.what();
        return std::nullopt;
    }
}

nats::service::jetstream_admin ClientManager::admin() {
    auto cl = session_.get_client();
    if (!cl)
        throw std::runtime_error("Not connected to NATS");
    return cl->make_admin();
}

std::optional<std::vector<iam::messaging::session_sample_dto>>
ClientManager::getSessionSamples(const boost::uuids::uuid& sessionId) {
    try {
        iam::messaging::get_session_samples_request request{
            .session_id = boost::uuids::to_string(sessionId)
        };
        auto result = process_authenticated_request(std::move(request));
        if (!result) {
            BOOST_LOG_SEV(lg(), error) << "getSessionSamples failed: " << result.error();
            return std::nullopt;
        }
        return std::move(result->samples);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "getSessionSamples failed: " << e.what();
        return std::nullopt;
    }
}

void ClientManager::arm_refresh_timer(int lifetime_s) {
    if (!refresh_timer_) return;
    const int fire_ms = static_cast<int>(lifetime_s * refresh_lifetime_ratio * 1000);
    refresh_timer_->stop();
    refresh_timer_->start(fire_ms);
    BOOST_LOG_SEV(lg(), debug) << "Refresh timer armed for " << (fire_ms / 1000) << "s";
}

void ClientManager::onRefreshTimer() {
    if (!session_.is_logged_in()) return;
    BOOST_LOG_SEV(lg(), info) << "Proactive JWT refresh triggered";
    try {
        auto result = process_authenticated_request(iam::messaging::refresh_request{});
        if (!result) {
            BOOST_LOG_SEV(lg(), warn) << "Proactive refresh failed: " << result.error();
            return;
        }
        if (!result->success || result->token.empty()) {
            BOOST_LOG_SEV(lg(), warn) << "Proactive refresh: server returned failure";
            return;
        }
        auto updated = session_.auth();
        updated.jwt = result->token;
        session_.set_auth(updated);
        arm_refresh_timer(result->access_lifetime_s);
        BOOST_LOG_SEV(lg(), info) << "Proactive JWT refresh succeeded";
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Proactive refresh exception: " << e.what();
    }
}

}
