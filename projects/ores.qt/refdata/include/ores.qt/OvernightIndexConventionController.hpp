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
#ifndef ORES_QT_OVERNIGHT_INDEX_CONVENTION_CONTROLLER_HPP
#define ORES_QT_OVERNIGHT_INDEX_CONVENTION_CONTROLLER_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/EntityController.hpp"
#include "ores.qt/EntityListMdiWindow.hpp"
#include "ores.refdata.api/domain/overnight_index_convention.hpp"
#include <QMainWindow>
#include <QMdiArea>
#include <expected>
#include <functional>
#include <vector>

namespace ores::qt {

class OvernightIndexConventionMdiWindow;
class DetachableMdiSubWindow;

/**
 * @brief Controller for managing overnight index convention windows and operations.
 *
 * Manages the lifecycle of overnight index convention list, detail, and history windows.
 * Handles event subscriptions and coordinates between windows.
 */
class OvernightIndexConventionController final : public EntityController {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.overnight_index_convention_controller";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    OvernightIndexConventionController(QMainWindow* mainWindow,
                                       QMdiArea* mdiArea,
                                       ClientManager* clientManager,
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
    void onShowDetails(const refdata::domain::overnight_index_convention& ni);
    void onAddNewRequested();
    void onShowHistory(const refdata::domain::overnight_index_convention& ni);
    void onRevertVersion(const refdata::domain::overnight_index_convention& ni);
    void onOpenVersion(const refdata::domain::overnight_index_convention& ni, int versionNumber);
    void onOpenHistoryVersion(const QString& entityId, int versionNumber);
    void onRevertHistoryVersion(const QString& entityId, int versionNumber);

private:
    void showAddWindow();
    void showDetailWindow(const refdata::domain::overnight_index_convention& ni);
    void showHistoryWindow(const QString& code);

    /**
     * @brief Fetches the full typed overnight index convention history (the
     * existing per-entity refdata::messaging::get_overnight_index_convention_history_request/
     * refdata::messaging::get_overnight_index_convention_history_response, unrelated to the generic
     * history.v1.get subject) and hands it to @p callback on the UI
     * thread. Used to resolve HistoryDialog's generic (entity_id,
     * version) signals back to a typed overnight index convention, since the
     * generic dialog holds no typed domain data.
     */
    void fetchOvernightIndexConventionHistory(
        const QString& entityId,
        std::function<void(std::expected<std::vector<refdata::domain::overnight_index_convention>,
                                         QString>)> callback);

    OvernightIndexConventionMdiWindow* listWindow_;
    DetachableMdiSubWindow* listMdiSubWindow_;
};

}

#endif
