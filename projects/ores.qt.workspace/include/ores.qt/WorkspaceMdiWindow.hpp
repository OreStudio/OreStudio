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
#ifndef ORES_QT_WORKSPACE_MDI_WINDOW_HPP
#define ORES_QT_WORKSPACE_MDI_WINDOW_HPP

#include <vector>
#include <unordered_map>
#include <QToolBar>
#include <QTreeWidget>
#include <QTreeWidgetItem>
#include "ores.qt/EntityListMdiWindow.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/ClientWorkspaceModel.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.workspace.api/domain/workspace.hpp"

namespace ores::qt {

/**
 * @brief MDI window for displaying and managing workspaces.
 *
 * Provides a tree view of workspaces with toolbar actions
 * for reload, add, open, edit, archive, and delete.
 */
class WorkspaceMdiWindow final : public EntityListMdiWindow {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.workspace_mdi_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit WorkspaceMdiWindow(
        ClientManager* clientManager,
        const QString& username,
        QWidget* parent = nullptr);
    ~WorkspaceMdiWindow() override = default;

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);
    void showWorkspaceDetails(const workspace::domain::workspace& workspace);
    void addNewRequested();
    void workspaceDeleted(const QString& code);
    void workspaceActivated(const workspace::domain::workspace& workspace);

public slots:
    void addNew();
    void editSelected();
    void archiveSelected();
    void deleteSelected();
    void openSelected();

protected:
    void doReload() override;

private slots:
    void onDataLoaded();
    void onLoadError(const QString& error_message, const QString& details = {});
    void onSelectionChanged();
    void onItemDoubleClicked(QTreeWidgetItem* item, int column);

protected:
    QString normalRefreshTooltip() const override {
        return tr("Refresh workspaces");
    }

private:
    void setupUi();
    void setupToolbar();
    void setupTree();
    void setupConnections();
    void updateActionStates();
    void buildTree();

    ClientManager* clientManager_;
    QString username_;

    QToolBar* toolbar_;
    QTreeWidget* treeWidget_;
    ClientWorkspaceModel* model_;

    // Toolbar actions
    QAction* reloadAction_;
    QAction* addAction_;
    QAction* openAction_;
    QAction* editAction_;
    QAction* archiveAction_;
    QAction* deleteAction_;
};

}

#endif
