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
#include "ores.qt/SettingGatedActionController.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.variability.api/eventing/system_setting_changed_event.hpp"
#include "ores.variability.api/messaging/system_settings_protocol.hpp"
#include <QtConcurrent>
#include <algorithm>

namespace ores::qt {

using namespace ores::logging;

namespace {
constexpr std::string_view system_setting_event_name =
    eventing::domain::event_traits<variability::eventing::system_setting_changed_event>::name;
}

SettingGatedActionController::SettingGatedActionController(ClientManager* client_manager,
                                                           QObject* parent)
    : QObject(parent)
    , clientManager_(client_manager) {

    if (!clientManager_)
        return;

    connect(clientManager_,
            &ClientManager::notificationReceived,
            this,
            &SettingGatedActionController::onNotification);

    connect(clientManager_, &ClientManager::loggedIn, this, [this]() {
        clientManager_->subscribeToEvent(std::string{system_setting_event_name});
        refresh();
    });

    connect(clientManager_, &ClientManager::reconnected, this, [this]() {
        clientManager_->subscribeToEvent(std::string{system_setting_event_name});
        refresh();
    });

    if (clientManager_->isLoggedIn()) {
        clientManager_->subscribeToEvent(std::string{system_setting_event_name});
    }
}

void SettingGatedActionController::registerAction(QAction* action,
                                                  const QString& setting_name,
                                                  std::function<bool()> guard,
                                                  bool default_when_missing) {
    if (!action)
        return;

    action->setVisible(false);
    actions_.push_back(
        {QPointer<QAction>(action), setting_name, std::move(guard), default_when_missing});
}

void SettingGatedActionController::refresh() {
    if (!clientManager_ || !clientManager_->isLoggedIn()) {
        for (auto& gated : actions_) {
            if (gated.action)
                gated.action->setVisible(false);
        }
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Refreshing " << actions_.size() << " setting-gated action(s)";

    auto client_manager = clientManager_;
    QFuture<std::vector<variability::domain::system_setting>> future =
        QtConcurrent::run([client_manager]() -> std::vector<variability::domain::system_setting> {
            variability::messaging::list_settings_request request;
            auto result = client_manager->process_authenticated_request(std::move(request));
            if (!result)
                return {};
            return result->settings;
        });

    auto* watcher = new QFutureWatcher<std::vector<variability::domain::system_setting>>(this);
    connect(watcher,
            &QFutureWatcher<std::vector<variability::domain::system_setting>>::finished,
            this,
            [this, watcher]() {
                const auto settings = watcher->result();
                watcher->deleteLater();

                for (auto& gated : actions_) {
                    if (!gated.action)
                        continue;

                    const auto it =
                        std::find_if(settings.begin(), settings.end(), [&gated](const auto& s) {
                            return s.name == gated.setting_name.toStdString();
                        });

                    bool enabled =
                        it != settings.end() ? it->value == "true" : gated.default_when_missing;
                    if (it == settings.end()) {
                        BOOST_LOG_SEV(lg(), debug)
                            << "System setting not found: " << gated.setting_name.toStdString()
                            << ", defaulting visibility to " << enabled;
                    }
                    if (enabled && gated.guard && !gated.guard())
                        enabled = false;
                    gated.action->setVisible(enabled);
                    BOOST_LOG_SEV(lg(), info)
                        << "Action visibility for setting " << gated.setting_name.toStdString()
                        << " set to: " << enabled;
                }
            });
    watcher->setFuture(future);
}

void SettingGatedActionController::onNotification(const QString& eventType,
                                                  const QDateTime&,
                                                  const QStringList& entityIds) {
    if (eventType != QString::fromStdString(std::string{system_setting_event_name}))
        return;

    const bool affects_us =
        entityIds.isEmpty() ||
        std::any_of(actions_.begin(), actions_.end(), [&entityIds](const auto& gated) {
            return entityIds.contains(gated.setting_name);
        });
    if (!affects_us)
        return;

    BOOST_LOG_SEV(lg(), info)
        << "System setting notification received, refreshing gated action visibility";
    refresh();
}

}
