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
#ifndef ORES_QT_CONNECTION_BROWSER_MDI_WINDOW_HPP
#define ORES_QT_CONNECTION_BROWSER_MDI_WINDOW_HPP

#include <QWidget>
#include <QTreeView>
#include <QToolBar>
#include <QAction>
#include <QVBoxLayout>
#include <functional>
#include <memory>
#include <boost/uuid/uuid.hpp>
#include "ores.logging/make_logger.hpp"

namespace ores::connections::service {
class connection_manager;
}

namespace ores::qt {

class ConnectionTreeModel;

/**
 * @brief Callback type for testing connections.
 */
using TestConnectionCallback = std::function<QString(
    const QString& host, int port, const QString& username, const QString& password)>;

/**
 * @brief MDI window for browsing and managing saved server connections.
 *
 * Provides a tree view of folders and server environments with toolbar
 * actions for CRUD operations. Works independently of server connection.
 */
class ConnectionBrowserMdiWindow : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.connection_browser_mdi_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit ConnectionBrowserMdiWindow(
        connections::service::connection_manager* manager,
        QWidget* parent = nullptr);
    ~ConnectionBrowserMdiWindow() override;

    QSize sizeHint() const override;

    /**
     * @brief Set callback for testing connections from dialogs.
     */
    void setTestCallback(TestConnectionCallback callback);

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& errorMessage);

    /**
     * @brief Emitted when user requests to connect using a saved environment.
     */
    void connectRequested(const boost::uuids::uuid& environmentId,
                          const QString& connectionName);

    /**
     * @brief Emitted when user requests to change the master password.
     */
    void changeMasterPasswordRequested();

    /**
     * @brief Emitted when user purges the database.
     */
    void databasePurged();

public slots:
    void reload();
    void createFolder();
    void createConnection();
    void editSelected();
    void deleteSelected();
    void connectToSelected();
    void changeMasterPassword();
    void purgeDatabase();

private slots:
    void onSelectionChanged();
    void onDoubleClicked(const QModelIndex& index);
    void showContextMenu(const QPoint& pos);

private:
    void setupUI();
    void updateActionStates();

    QVBoxLayout* layout_;
    QTreeView* treeView_;
    QToolBar* toolBar_;

    QAction* createFolderAction_;
    QAction* createConnectionAction_;
    QAction* editAction_;
    QAction* deleteAction_;
    QAction* connectAction_;
    QAction* refreshAction_;
    QAction* changeMasterPasswordAction_;
    QAction* purgeDatabaseAction_;

    std::unique_ptr<ConnectionTreeModel> model_;
    connections::service::connection_manager* manager_;
    TestConnectionCallback testCallback_;
};

}

#endif
