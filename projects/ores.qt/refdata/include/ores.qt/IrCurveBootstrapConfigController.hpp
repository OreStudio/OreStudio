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
#ifndef ORES_QT_IR_CURVE_BOOTSTRAP_CONFIG_CONTROLLER_HPP
#define ORES_QT_IR_CURVE_BOOTSTRAP_CONFIG_CONTROLLER_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/EntityController.hpp"
#include "ores.qt/EntityListMdiWindow.hpp"
#include "ores.qt/RefdataExport.hpp"
#include "ores.refdata.api/domain/ir_curve_bootstrap_config.hpp"
#include <QMainWindow>
#include <QMdiArea>
#include <expected>
#include <functional>
#include <vector>

namespace ores::qt {

class IrCurveBootstrapConfigMdiWindow;
class CurveBuilderWorkbench;
class DetachableMdiSubWindow;
class ImageCache;
class ChangeReasonCache;

/**
 * @brief Controller for managing IR curve bootstrap config windows and operations.
 *
 * Manages the lifecycle of IR curve bootstrap config list, detail, and history windows.
 * Handles event subscriptions and coordinates between windows.
 */
class ORES_QT_REFDATA_EXPORT IrCurveBootstrapConfigController final : public EntityController {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.ir_curve_bootstrap_config_controller";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    IrCurveBootstrapConfigController(QMainWindow* mainWindow,
                                     QMdiArea* mdiArea,
                                     ClientManager* clientManager,
                                     ImageCache* imageCache,
                                     ChangeReasonCache* changeReasonCache,
                                     const QString& username,
                                     QObject* parent = nullptr);

    void showListWindow() override;
    void closeAllWindows() override;
    void reloadListWindow() override;

    /// Opens a blank CurveBuilderWorkbench directly, bypassing the list window -- used by the
    /// Market Data menu's "Build Curve..." action, which is a shortcut into the same flow as the
    /// list window's own Add button (showAddWindow() stays private since it's normally reached
    /// only via onAddNewRequested()).
    void openNewCurveWindow();


signals:
    void statusMessage(const QString& message);
    void errorMessage(const QString& error);

protected:
    EntityListMdiWindow* listWindow() const override;
    void notifyOpenDialogs(const QStringList& entityIds) override;

private slots:
    void onShowDetails(const refdata::domain::ir_curve_bootstrap_config& config);
    void onAddNewRequested();
    void onShowHistory(const refdata::domain::ir_curve_bootstrap_config& config);
    void onRevertVersion(const refdata::domain::ir_curve_bootstrap_config& config);
    void onOpenVersion(const refdata::domain::ir_curve_bootstrap_config& config, int versionNumber);
    void onOpenHistoryVersion(const QString& entityId, int versionNumber);
    void onRevertHistoryVersion(const QString& entityId, int versionNumber);

private:
    void showAddWindow();
    void showDetailWindow(const refdata::domain::ir_curve_bootstrap_config& config);

    /**
     * @brief Wires the client manager/username/status/error/close plumbing every
     * CurveBuilderWorkbench needs regardless of which window opened it
     * (add/edit/history-version/revert) -- kept in one place so those four call sites can't
     * drift from each other. Hand-rolled rather than EntityController::connect_dialog_close()
     * (which needs a DetailDialogBase*) since this workbench deliberately doesn't inherit that
     * save-and-close-oriented base -- see CurveBuilderWorkbench's own class docs.
     */
    void wireWorkbenchCommon(CurveBuilderWorkbench* workbench, DetachableMdiSubWindow* window);
    void showHistoryWindow(const refdata::domain::ir_curve_bootstrap_config& config);

    /**
     * @brief Fetches the full typed IR curve bootstrap config history (the
     * existing per-entity refdata::messaging::get_ir_curve_bootstrap_config_history_request/
     * refdata::messaging::get_ir_curve_bootstrap_config_history_response, unrelated to the generic
     * history.v1.get subject) and hands it to @p callback on the UI
     * thread. Used to resolve HistoryDialog's generic (entity_id,
     * version) signals back to a typed IR curve bootstrap config, since the
     * generic dialog holds no typed domain data.
     */
    void fetchIrCurveBootstrapConfigHistory(
        const QString& entityId,
        std::function<void(std::expected<std::vector<refdata::domain::ir_curve_bootstrap_config>,
                                         QString>)> callback);

    IrCurveBootstrapConfigMdiWindow* listWindow_;
    DetachableMdiSubWindow* listMdiSubWindow_;
    ChangeReasonCache* changeReasonCache_;
};

}

#endif
