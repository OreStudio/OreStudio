/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_QT_SYSTEM_SETTING_MDI_WINDOW_HPP
#define ORES_QT_SYSTEM_SETTING_MDI_WINDOW_HPP

#include <QTableView>
#include <QVBoxLayout>
#include <QToolBar>
#include <QSortFilterProxyModel>
#include <memory>
#include "ores.qt/EntityListMdiWindow.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientSystemSettingModel.hpp"

namespace ores::qt {

/**
 * @brief MDI window for displaying and managing system settings.
 *
 * This window provides functionality for viewing, creating, editing,
 * and deleting system settings.
 */
class SystemSettingMdiWindow : public EntityListMdiWindow {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.system_setting_mdi_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit SystemSettingMdiWindow(ClientManager* clientManager,
                                  const QString& username,
                                  QWidget* parent = nullptr);
    ~SystemSettingMdiWindow() override;

    ClientSystemSettingModel* systemSettingModel() const { return systemSettingModel_.get(); }

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);
    void selectionChanged(int selection_count);
    void addNewRequested();
    void showSystemSettingDetails(const variability::domain::system_setting& flag);
    void showHistoryRequested(const QString& name);
    void systemSettingDeleted(const QString& name);

public slots:
    void doReload() override;
    void addNew();
    void editSelected();
    void deleteSelected();
    void showHistory();

protected:
    QString normalRefreshTooltip() const override {
        return tr("Refresh system settings");
    }

private slots:
    void onDataLoaded();
    void onLoadError(const QString& error_message, const QString& details = {});
    void onRowDoubleClicked(const QModelIndex& index);
    void onSelectionChanged();
    void onConnectionStateChanged();

private:
    void updateActionStates();
    void setupReloadAction();

private:
    QVBoxLayout* verticalLayout_;
    QTableView* systemSettingTableView_;
    QToolBar* toolBar_;

    QAction* reloadAction_;
    QAction* addAction_;
    QAction* editAction_;
    QAction* deleteAction_;
    QAction* historyAction_;

    std::unique_ptr<ClientSystemSettingModel> systemSettingModel_;
    QSortFilterProxyModel* proxyModel_;
    ClientManager* clientManager_;
    QString username_;
};

}

#endif
