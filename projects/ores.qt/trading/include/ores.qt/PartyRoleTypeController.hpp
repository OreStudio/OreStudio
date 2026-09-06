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
#ifndef ORES_QT_PARTY_ROLE_TYPE_CONTROLLER_HPP
#define ORES_QT_PARTY_ROLE_TYPE_CONTROLLER_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/EntityController.hpp"
#include "ores.qt/EntityListMdiWindow.hpp"
#include "ores.trading.api/domain/party_role_type.hpp"
#include <QMainWindow>
#include <QMdiArea>
#include <expected>
#include <functional>
#include <vector>

namespace ores::qt {

class PartyRoleTypeMdiWindow;
class PartyRoleTypeDetailDialog;
class DetachableMdiSubWindow;
class ChangeReasonCache;

/**
 * @brief Controller for managing party role type windows and operations.
 *
 * Manages the lifecycle of party role type list, detail, and history windows.
 * Handles event subscriptions and coordinates between windows.
 */
class PartyRoleTypeController final : public EntityController {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.party_role_type_controller";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    PartyRoleTypeController(QMainWindow* mainWindow,
                            QMdiArea* mdiArea,
                            ClientManager* clientManager,
                            ChangeReasonCache* changeReasonCache,
                            const QString& username,
                            QObject* parent = nullptr);

    void showListWindow() override;
    void closeAllWindows() override;
    void reloadListWindow() override;


signals:
    void statusMessage(const QString& message);
    void errorMessage(const QString& error);

protected:
    EntityListMdiWindow* listWindow() const override;
    void notifyOpenDialogs(const QStringList& entityIds) override;

private slots:
    void onShowDetails(const trading::domain::party_role_type& party_role_type);
    void onAddNewRequested();
    void onShowHistory(const trading::domain::party_role_type& party_role_type);
    void onRevertVersion(const trading::domain::party_role_type& party_role_type);
    void onOpenVersion(const trading::domain::party_role_type& party_role_type, int versionNumber);
    void onOpenHistoryVersion(const QString& entityId, int versionNumber);
    void onRevertHistoryVersion(const QString& entityId, int versionNumber);

private:
    void showAddWindow();
    void showDetailWindow(const trading::domain::party_role_type& party_role_type);

    /**
     * @brief Wires the caches/status/error plumbing every
     * PartyRoleTypeDetailDialog needs regardless of which
     * window opened it (add/edit/history-version/revert) -- kept in one
     * place so those four call sites can't drift from each other.
     */
    void wireDetailDialogCommon(PartyRoleTypeDetailDialog* detailDialog);
    void showHistoryWindow(const QString& code);

    /**
     * @brief Fetches the full typed party role type history (the
     * existing per-entity trading::messaging::get_party_role_type_history_request/
     * trading::messaging::get_party_role_type_history_response, unrelated to the generic
     * history.v1.get subject) and hands it to @p callback on the UI
     * thread. Used to resolve HistoryDialog's generic (entity_id,
     * version) signals back to a typed party role type, since the
     * generic dialog holds no typed domain data.
     */
    void fetchPartyRoleTypeHistory(
        const QString& entityId,
        std::function<void(std::expected<std::vector<trading::domain::party_role_type>, QString>)>
            callback);

    ChangeReasonCache* changeReasonCache_;
    PartyRoleTypeMdiWindow* listWindow_;
    DetachableMdiSubWindow* listMdiSubWindow_;
};

}

#endif
