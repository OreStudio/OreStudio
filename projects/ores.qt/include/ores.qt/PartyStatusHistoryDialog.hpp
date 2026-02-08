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
#ifndef ORES_QT_PARTY_STATUS_HISTORY_DIALOG_HPP
#define ORES_QT_PARTY_STATUS_HISTORY_DIALOG_HPP

#include <QWidget>
#include <QToolBar>
#include <QTableWidget>
#include "ores.qt/ClientManager.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata/domain/party_status.hpp"

namespace Ui {
class PartyStatusHistoryDialog;
}

namespace ores::qt {

/**
 * @brief Dialog for viewing the version history of a party status.
 *
 * Shows all historical versions of a party status with ability
 * to view details or revert to a previous version.
 */
class PartyStatusHistoryDialog final : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.party_status_history_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit PartyStatusHistoryDialog(
        const QString& code,
        ClientManager* clientManager,
        QWidget* parent = nullptr);
    ~PartyStatusHistoryDialog() override;

    void loadHistory();

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);
    void openVersionRequested(const refdata::domain::party_status& status,
                              int versionNumber);
    void revertVersionRequested(const refdata::domain::party_status& status);

private slots:
    void onVersionSelected();
    void onOpenVersionClicked();
    void onRevertClicked();

private:
    void setupUi();
    void setupToolbar();
    void setupConnections();
    void updateVersionList();
    void updateChangesTable(int currentVersionIndex);
    void updateFullDetails(int versionIndex);
    void updateActionStates();

    Ui::PartyStatusHistoryDialog* ui_;
    QString code_;
    ClientManager* clientManager_;
    std::vector<refdata::domain::party_status> versions_;

    QToolBar* toolbar_;
    QAction* openVersionAction_;
    QAction* revertAction_;
};

}

#endif
