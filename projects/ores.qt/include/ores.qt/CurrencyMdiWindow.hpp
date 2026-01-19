/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_QT_CURRENCY_MDI_WINDOW_HPP
#define ORES_QT_CURRENCY_MDI_WINDOW_HPP

#include <QTableView>
#include <QVBoxLayout>
#include <QToolBar>
#include <QSortFilterProxyModel>
#include <QCloseEvent>
#include <memory>
#include "ores.qt/EntityListMdiWindow.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientCurrencyModel.hpp"
#include "ores.qt/PaginationWidget.hpp"

namespace ores::qt {

class ImageCache;

/**
 * @brief MDI window for displaying currencies.
 */
class CurrencyMdiWindow : public EntityListMdiWindow {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.currency_mdi_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit CurrencyMdiWindow(ClientManager* clientManager,
                               ImageCache* imageCache,
                               const QString& username,
                               QWidget* parent = nullptr);
    ~CurrencyMdiWindow() override;

    ClientCurrencyModel* currencyModel() const { return currencyModel_.get(); }

    QSize sizeHint() const override;

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);
    void selectionChanged(int selection_count);
    void addNewRequested();
    void showCurrencyDetails(const refdata::domain::currency& currency);
    void currencyDeleted(const QString& iso_code);
    void showCurrencyHistory(const QString& iso_code);

public slots:
    void reload() override;
    void addNew();
    void editSelected();
    void deleteSelected();
    void viewHistorySelected();
    void importFromXML();
    void exportToCSV();
    void exportToXML();
    void generateSynthetic();

private slots:
    void onDataLoaded();
    void onLoadError(const QString& error_message, const QString& details = {});
    void onRowDoubleClicked(const QModelIndex& index);
    void onSelectionChanged();
    void onConnectionStateChanged();
    void onFeatureFlagNotification(const QString& eventType, const QDateTime& timestamp,
                                    const QStringList& entityIds);

protected:
    QString normalRefreshTooltip() const override {
        return tr("Refresh currencies");
    }

private:
    void updateActionStates();
    void setupReloadAction();
    void setupGenerateAction();
    void updateGenerateActionVisibility();
    void setupColumnVisibility();
    void showHeaderContextMenu(const QPoint& pos);
    void saveSettings();
    void restoreSettings();

protected:
    void closeEvent(QCloseEvent* event) override;

private:
    QVBoxLayout* verticalLayout_;
    QTableView* currencyTableView_;
    QToolBar* toolBar_;
    PaginationWidget* pagination_widget_;

    // Reload action with stale indicator
    QAction* reloadAction_;

    QAction* addAction_;
    QAction* editAction_;
    QAction* deleteAction_;
    QAction* historyAction_;
    QAction* generateAction_;

    std::unique_ptr<ClientCurrencyModel> currencyModel_;
    QSortFilterProxyModel* proxyModel_;
    ClientManager* clientManager_;
    ImageCache* imageCache_;
    QString username_;
};

}

#endif
