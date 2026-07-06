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
#ifndef ORES_QT_SETTING_GATED_ACTION_CONTROLLER_HPP
#define ORES_QT_SETTING_GATED_ACTION_CONTROLLER_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.qt/export.hpp"
#include <QAction>
#include <QDateTime>
#include <QObject>
#include <QPointer>
#include <QString>
#include <QStringList>
#include <vector>

namespace ores::qt {

class ClientManager;

/**
 * @brief Shows/hides QActions based on boolean system settings.
 *
 * Generalises the mechanism that grew organically in currency's
 * "generate synthetic test data" button: subscribe to the system
 * setting change NATS event, and on login/reconnect/notification,
 * batch-fetch all settings and set each registered action's
 * visibility from its matching setting's boolean value.
 *
 * One controller instance can gate several actions across different
 * settings — a single NATS subscription and a single list-settings
 * fetch covers all of them, rather than one per gated action.
 */
class ORES_QT_API SettingGatedActionController final : public QObject {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.setting_gated_action_controller";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit SettingGatedActionController(ClientManager* client_manager,
                                          QObject* parent = nullptr);

    /**
     * @brief Registers an action to be gated by a boolean system setting.
     *
     * The action starts hidden; call refresh() (or wait for
     * login/reconnect, which this class wires automatically) to
     * evaluate its initial visibility.
     */
    void registerAction(QAction* action, const QString& setting_name);

    /**
     * @brief Re-checks every registered action's visibility.
     *
     * Safe to call before login; the check is a no-op until the
     * client manager reports being logged in.
     */
    void refresh();

private slots:
    void onNotification(const QString& eventType,
                        const QDateTime& timestamp,
                        const QStringList& entityIds);

private:
    struct GatedAction {
        QPointer<QAction> action;
        QString setting_name;
    };

    ClientManager* clientManager_;
    std::vector<GatedAction> actions_;
};

}

#endif
