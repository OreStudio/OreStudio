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
#ifndef ORES_QT_BOOK_MDI_WINDOW_HPP
#define ORES_QT_BOOK_MDI_WINDOW_HPP

#include <QToolBar>
#include <QTableView>
#include <QSortFilterProxyModel>
#include "ores.qt/EntityListMdiWindow.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/ClientBookModel.hpp"
#include "ores.qt/PaginationWidget.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata/domain/book.hpp"

namespace ores::qt {

/**
 * @brief MDI window for displaying and managing books.
 *
 * Provides a table view of books with toolbar actions
 * for reload, add, edit, delete, and viewing history.
 */
class BookMdiWindow final : public EntityListMdiWindow {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.book_mdi_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit BookMdiWindow(
        ClientManager* clientManager,
        const QString& username,
        QWidget* parent = nullptr);
    ~BookMdiWindow() override = default;

    QSize sizeHint() const override;

public slots:
    void reload() override;

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);
    void showBookDetails(const refdata::domain::book& book);
    void addNewRequested();
    void showBookHistory(const refdata::domain::book& book);

public slots:
    void addNew();
    void editSelected();
    void deleteSelected();
    void viewHistorySelected();

private slots:
    void onDataLoaded();
    void onLoadError(const QString& error_message, const QString& details = {});
    void onSelectionChanged();
    void onDoubleClicked(const QModelIndex& index);

private slots:
    void showHeaderContextMenu(const QPoint& pos);

protected:
    QString normalRefreshTooltip() const override {
        return tr("Refresh books");
    }

private:
    void setupUi();
    void setupToolbar();
    void setupTable();
    void setupColumnVisibility();
    void setupConnections();
    void updateActionStates();
    void saveSettings() override;
    void restoreSettings();

    ClientManager* clientManager_;
    QString username_;

    QToolBar* toolbar_;
    QTableView* tableView_;
    ClientBookModel* model_;
    QSortFilterProxyModel* proxyModel_;
    PaginationWidget* paginationWidget_;

    // Toolbar actions
    QAction* reloadAction_;
    QAction* addAction_;
    QAction* editAction_;
    QAction* deleteAction_;
    QAction* historyAction_;
};

}

#endif
