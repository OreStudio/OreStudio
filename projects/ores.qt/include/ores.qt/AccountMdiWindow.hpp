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
#ifndef ORES_QT_ACCOUNT_MDI_WINDOW_HPP
#define ORES_QT_ACCOUNT_MDI_WINDOW_HPP

#include <QWidget>
#include <QTableView>
#include <QVBoxLayout>
#include <QToolBar>
#include <QIcon>
#include <QTimer>
#include <QSortFilterProxyModel>
#include <memory>
#include "ores.qt/ClientManager.hpp"
#include "ores.utility/log/make_logger.hpp"
#include "ores.qt/ClientAccountModel.hpp"
#include "ores.qt/PaginationWidget.hpp"

namespace ores::qt {

/**
 * @brief MDI window for displaying and managing user accounts.
 *
 * This window is only accessible to admin users and provides functionality
 * for viewing, creating, editing, deleting, and locking/unlocking accounts.
 */
class AccountMdiWindow : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.account_mdi_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit AccountMdiWindow(ClientManager* clientManager,
                              const QString& username,
                              QWidget* parent = nullptr);
    ~AccountMdiWindow() override;

    ClientAccountModel* accountModel() const { return accountModel_.get(); }

    QSize sizeHint() const override;

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);
    void selectionChanged(int selection_count);
    void addNewRequested();
    void showAccountDetails(const AccountWithLoginInfo& accountWithLoginInfo);
    void accountDeleted(const boost::uuids::uuid& account_id);

public slots:
    void reload();
    void addNew();
    void editSelected();
    void deleteSelected();
    void lockSelected();
    void unlockSelected();

    /**
     * @brief Mark the data as stale (changed on server).
     *
     * Shows a visual indicator that data may be out of date and
     * should be refreshed. Called when an account change notification
     * is received from the server.
     */
    void markAsStale();

    /**
     * @brief Clear the stale indicator.
     *
     * Called after data is reloaded to indicate data is fresh.
     */
    void clearStaleIndicator();

private slots:
    void onDataLoaded();
    void onLoadError(const QString& error_message);
    void onRowDoubleClicked(const QModelIndex& index);
    void onSelectionChanged();
    void onConnectionStateChanged();

private:
    void updateActionStates();
    void setupReloadAction();
    void startPulseAnimation();
    void stopPulseAnimation();

private:
    QVBoxLayout* verticalLayout_;
    QTableView* accountTableView_;
    QToolBar* toolBar_;
    PaginationWidget* pagination_widget_;

    // Reload action with stale indicator
    QAction* reloadAction_;
    QIcon normalReloadIcon_;
    QIcon staleReloadIcon_;
    QTimer* pulseTimer_;
    bool pulseState_{false};
    int pulseCount_{0};

    QAction* addAction_;
    QAction* editAction_;
    QAction* deleteAction_;
    QAction* lockAction_;
    QAction* unlockAction_;

    std::unique_ptr<ClientAccountModel> accountModel_;
    QSortFilterProxyModel* proxyModel_;
    ClientManager* clientManager_;
    QString username_;
    bool isStale_{false};
};

}

#endif
