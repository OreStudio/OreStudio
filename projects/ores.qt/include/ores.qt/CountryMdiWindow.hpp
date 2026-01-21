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
#ifndef ORES_QT_COUNTRY_MDI_WINDOW_HPP
#define ORES_QT_COUNTRY_MDI_WINDOW_HPP

#include <QTableView>
#include <QVBoxLayout>
#include <QToolBar>
#include <QSortFilterProxyModel>
#include <QCloseEvent>
#include <memory>
#include "ores.qt/EntityListMdiWindow.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientCountryModel.hpp"
#include "ores.qt/PaginationWidget.hpp"

namespace ores::qt {

class ImageCache;

/**
 * @brief MDI window for displaying countries.
 */
class CountryMdiWindow : public EntityListMdiWindow {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.country_mdi_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit CountryMdiWindow(ClientManager* clientManager,
                              ImageCache* imageCache,
                              const QString& username,
                              QWidget* parent = nullptr);
    ~CountryMdiWindow() override;

    ClientCountryModel* countryModel() const { return countryModel_.get(); }

    QSize sizeHint() const override;

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);
    void selectionChanged(int selection_count);
    void addNewRequested();
    void showCountryDetails(const refdata::domain::country& country);
    void countryDeleted(const QString& alpha2_code);
    void showCountryHistory(const QString& alpha2_code);

public slots:
    void reload() override;
    void addNew();
    void editSelected();
    void deleteSelected();
    void viewHistorySelected();
    void exportToCSV();

private slots:
    void onDataLoaded();
    void onLoadError(const QString& error_message, const QString& details = {});
    void onRowDoubleClicked(const QModelIndex& index);
    void onSelectionChanged();
    void onConnectionStateChanged();

protected:
    QString normalRefreshTooltip() const override { return tr("Refresh countries"); }

private:
    void updateActionStates();
    void setupReloadAction();
    void setupColumnVisibility();
    void showHeaderContextMenu(const QPoint& pos);
    void saveSettings();
    void restoreSettings();

protected:
    void closeEvent(QCloseEvent* event) override;

private:
    QVBoxLayout* verticalLayout_;
    QTableView* countryTableView_;
    QToolBar* toolBar_;
    PaginationWidget* pagination_widget_;

    // Reload action with stale indicator
    QAction* reloadAction_;

    QAction* addAction_;
    QAction* editAction_;
    QAction* deleteAction_;
    QAction* historyAction_;

    std::unique_ptr<ClientCountryModel> countryModel_;
    QSortFilterProxyModel* proxyModel_;
    ClientManager* clientManager_;
    ImageCache* imageCache_;
    QString username_;
};

}

#endif
