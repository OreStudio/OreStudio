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
#ifndef ORES_QT_PRICING_MODEL_CONFIG_CONTROLLER_HPP
#define ORES_QT_PRICING_MODEL_CONFIG_CONTROLLER_HPP

#include "ores.analytics.api/domain/pricing_model_config.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/EntityController.hpp"
#include "ores.qt/EntityListMdiWindow.hpp"
#include <QMainWindow>
#include <QMdiArea>
#include <expected>
#include <functional>
#include <vector>

namespace ores::qt {

class PricingModelConfigMdiWindow;
class PricingModelConfigDetailDialog;
class DetachableMdiSubWindow;
class ChangeReasonCache;

/**
 * @brief Controller for managing pricing model configuration windows and operations.
 *
 * Manages the lifecycle of pricing model configuration list, detail, and history windows.
 * Handles event subscriptions and coordinates between windows.
 */
class PricingModelConfigController final : public EntityController {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.pricing_model_config_controller";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    PricingModelConfigController(QMainWindow* mainWindow,
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
    void onShowDetails(const analytics::domain::pricing_model_config& config);
    void onAddNewRequested();
    void onShowHistory(const analytics::domain::pricing_model_config& config);
    void onRevertVersion(const analytics::domain::pricing_model_config& config);
    void onOpenVersion(const analytics::domain::pricing_model_config& config, int versionNumber);
    void onOpenHistoryVersion(const QString& entityId, int versionNumber);
    void onRevertHistoryVersion(const QString& entityId, int versionNumber);

private:
    void showAddWindow();
    void showDetailWindow(const analytics::domain::pricing_model_config& config);

    /**
     * @brief Wires the caches/status/error plumbing every
     * PricingModelConfigDetailDialog needs regardless of which
     * window opened it (add/edit/history-version/revert) -- kept in one
     * place so those four call sites can't drift from each other.
     */
    void wireDetailDialogCommon(PricingModelConfigDetailDialog* detailDialog);
    void showHistoryWindow(const analytics::domain::pricing_model_config& config);

    /**
     * @brief Fetches the full typed pricing model configuration history (the
     * existing per-entity analytics::messaging::get_pricing_model_config_history_request/
     * analytics::messaging::get_pricing_model_config_history_response, unrelated to the generic
     * history.v1.get subject) and hands it to @p callback on the UI
     * thread. Used to resolve HistoryDialog's generic (entity_id,
     * version) signals back to a typed pricing model configuration, since the
     * generic dialog holds no typed domain data.
     */
    void fetchPricingModelConfigHistory(
        const QString& entityId,
        std::function<void(std::expected<std::vector<analytics::domain::pricing_model_config>,
                                         QString>)> callback);

    ChangeReasonCache* changeReasonCache_;
    PricingModelConfigMdiWindow* listWindow_;
    DetachableMdiSubWindow* listMdiSubWindow_;
};

}

#endif
