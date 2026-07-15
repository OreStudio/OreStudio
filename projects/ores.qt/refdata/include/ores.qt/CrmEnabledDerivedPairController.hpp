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
#ifndef ORES_QT_CRM_ENABLED_DERIVED_PAIR_CONTROLLER_HPP
#define ORES_QT_CRM_ENABLED_DERIVED_PAIR_CONTROLLER_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/EntityController.hpp"
#include "ores.qt/EntityListMdiWindow.hpp"
#include "ores.qt/RefdataExport.hpp"
#include "ores.refdata.api/domain/crm_enabled_derived_pair.hpp"
#include <QMainWindow>
#include <QMdiArea>
#include <expected>
#include <functional>
#include <vector>

namespace ores::qt {

class CrmEnabledDerivedPairMdiWindow;
class DetachableMdiSubWindow;
class BadgeCache;
class ChangeReasonCache;
class ImageCache;

/**
 * @brief Controller for managing CRM enabled derived pair windows and operations.
 *
 * Manages the lifecycle of CRM enabled derived pair list, detail, and history windows.
 * Handles event subscriptions and coordinates between windows.
 */
class ORES_QT_REFDATA_EXPORT CrmEnabledDerivedPairController final : public EntityController {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.crm_enabled_derived_pair_controller";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    CrmEnabledDerivedPairController(QMainWindow* mainWindow,
                                    QMdiArea* mdiArea,
                                    ClientManager* clientManager,
                                    ImageCache* imageCache,
                                    ChangeReasonCache* changeReasonCache,
                                    const QString& username,
                                    BadgeCache* badgeCache,
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
    void onShowDetails(const refdata::domain::crm_enabled_derived_pair& pair);
    void onAddNewRequested();
    void onShowHistory(const refdata::domain::crm_enabled_derived_pair& pair);
    void onRevertVersion(const refdata::domain::crm_enabled_derived_pair& pair);
    void onOpenVersion(const refdata::domain::crm_enabled_derived_pair& pair, int versionNumber);
    void onOpenHistoryVersion(const QString& entityId, int versionNumber);
    void onRevertHistoryVersion(const QString& entityId, int versionNumber);

private:
    void showAddWindow();
    void showDetailWindow(const refdata::domain::crm_enabled_derived_pair& pair);
    void showHistoryWindow(const refdata::domain::crm_enabled_derived_pair& pair);

    /**
     * @brief Fetches the full typed CRM enabled derived pair history (the
     * existing per-entity refdata::messaging::get_crm_enabled_derived_pair_history_request/
     * refdata::messaging::get_crm_enabled_derived_pair_history_response, unrelated to the generic
     * history.v1.get subject) and hands it to @p callback on the UI
     * thread. Used to resolve HistoryDialog's generic (entity_id,
     * version) signals back to a typed CRM enabled derived pair, since the
     * generic dialog holds no typed domain data.
     */
    void fetchCrmEnabledDerivedPairHistory(
        const QString& entityId,
        std::function<void(std::expected<std::vector<refdata::domain::crm_enabled_derived_pair>,
                                         QString>)> callback);

    ChangeReasonCache* changeReasonCache_;
    BadgeCache* badgeCache_;
    CrmEnabledDerivedPairMdiWindow* listWindow_;
    DetachableMdiSubWindow* listMdiSubWindow_;
};

}

#endif
