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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_QT_PARTY_PLUGIN_HPP
#define ORES_QT_PARTY_PLUGIN_HPP

#include <memory>
#include <QObject>
#include <QList>
#include "ores.qt/IPlugin.hpp"

namespace ores::qt {

class DetachableMdiSubWindow;
class PartyTypeController;
class PartyStatusController;
class PartyIdSchemeController;
class ContactTypeController;
class PartyController;
class CounterpartyController;
class BusinessCentreController;
class BusinessUnitController;
class BusinessUnitTypeController;

/**
 * @brief Party/organisation plugin: parties, counterparties, business
 *        centres, business units, and related type tables.
 *
 * Extracted from LegacyPlugin in Step 6 of the Qt plugin refactor.
 * Owns the Organization menu.
 */
class PartyPlugin : public QObject, public IPlugin {
    Q_OBJECT

public:
    explicit PartyPlugin(QObject* parent = nullptr);
    ~PartyPlugin() override;

    QString name() const override { return QStringLiteral("ores.qt.party"); }
    int load_order() const override { return 400; }

    void on_login(const plugin_context& ctx) override;
    QList<QMenu*> create_menus() override;
    void on_logout() override;

signals:
    /** @brief Forwarded status/error messages from entity controllers. */
    void status_message(const QString& msg);

    /** @brief Forwarded window lifecycle signals from entity controllers. */
    void window_created(DetachableMdiSubWindow* window);
    void window_destroyed(DetachableMdiSubWindow* window);

private slots:
    void on_status_message(const QString& msg) { emit status_message(msg); }
    void on_window_created(DetachableMdiSubWindow* w) { emit window_created(w); }
    void on_window_destroyed(DetachableMdiSubWindow* w) { emit window_destroyed(w); }

private:
    void connect_controller_signals(QObject* ctrl);

    plugin_context ctx_;

    std::unique_ptr<PartyTypeController>         partyTypeController_;
    std::unique_ptr<PartyStatusController>       partyStatusController_;
    std::unique_ptr<PartyIdSchemeController>     partyIdSchemeController_;
    std::unique_ptr<ContactTypeController>       contactTypeController_;
    std::unique_ptr<PartyController>             partyController_;
    std::unique_ptr<CounterpartyController>      counterpartyController_;
    std::unique_ptr<BusinessCentreController>    businessCentreController_;
    std::unique_ptr<BusinessUnitController>      businessUnitController_;
    std::unique_ptr<BusinessUnitTypeController>  businessUnitTypeController_;
};

}

#endif
