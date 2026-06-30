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
#ifndef ORES_QT_FX_SPOT_GENERATION_CONFIG_MDI_WINDOW_HPP
#define ORES_QT_FX_SPOT_GENERATION_CONFIG_MDI_WINDOW_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientFxSpotGenerationConfigModel.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/EntityListMdiWindow.hpp"
#include "ores.qt/PaginationWidget.hpp"
#include "ores.synthetic.api/domain/fx_spot_generation_config.hpp"
#include <QSortFilterProxyModel>
#include <QTableView>
#include <QToolBar>

namespace ores::qt {

/**
 * @brief MDI window for displaying and managing FX spot generation configs.
 *
 * Provides a table view of FX spot generation configs with toolbar actions
 * for reload, add, edit, and delete.
 */
class FxSpotGenerationConfigMdiWindow final : public EntityListMdiWindow {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.fx_spot_generation_config_mdi_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit FxSpotGenerationConfigMdiWindow(ClientManager* clientManager,
                                             const QString& username,
                                             QWidget* parent = nullptr);
    ~FxSpotGenerationConfigMdiWindow() override = default;

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);
    void showFxSpotConfigDetails(const synthetic::domain::fx_spot_generation_config& config);
    void addNewRequested();
    void fxSpotConfigDeleted(const QString& id);

public slots:
    void addNew();
    void editSelected();
    void deleteSelected();

protected:
    void doReload() override;

private slots:
    void onDataLoaded();
    void onLoadError(const QString& error_message, const QString& details = {});
    void onSelectionChanged();
    void onDoubleClicked(const QModelIndex& index);

protected:
    QString normalRefreshTooltip() const override {
        return tr("Refresh FX spot generation configs");
    }

private:
    void setupUi();
    void setupToolbar();
    void setupTable();
    void setupConnections();
    void updateActionStates();

    ClientManager* clientManager_;
    QString username_;

    QToolBar* toolbar_;
    QTableView* tableView_;
    ClientFxSpotGenerationConfigModel* model_;
    QSortFilterProxyModel* proxyModel_;
    PaginationWidget* paginationWidget_;

    // Toolbar actions
    QAction* reloadAction_;
    QAction* addAction_;
    QAction* editAction_;
    QAction* deleteAction_;
};

}

#endif
