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
#ifndef ORES_QT_ORE_LOG_VIEWER_WIDGET_HPP
#define ORES_QT_ORE_LOG_VIEWER_WIDGET_HPP

#include <memory>
#include <QString>
#include <QWidget>
#include <QTabWidget>
#include <QTableView>
#include <QTextEdit>
#include <QSplitter>
#include <QLabel>
#include <QSortFilterProxyModel>
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/ClientTelemetryLogModel.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::qt {

/**
 * @brief Displays ORE engine and wrapper logs for a single compute result.
 *
 * Owns one ClientTelemetryLogModel filtered by result tag.  Two
 * QSortFilterProxyModel instances provide per-source tab views ("ORE Engine"
 * and "Wrapper") without additional network requests.  A third "All" tab
 * shows the combined stream.  Selecting a row populates a detail pane below.
 */
class OreLogViewerWidget : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.ore_log_viewer_widget";
    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit OreLogViewerWidget(ClientManager* clientManager,
                                QWidget* parent = nullptr);

    /**
     * @brief Load logs for the given result id (UUID string).
     *
     * Sets the tag filter on the underlying model and triggers a fetch.
     */
    void load_result(const QString& result_id);

    /**
     * @brief Clear all displayed log entries.
     */
    void clear();

public slots:
    void refresh();

private slots:
    void on_data_loaded();
    void on_load_error(const QString& message, const QString& details);
    void on_selection_changed(const QItemSelection& selected,
                              const QItemSelection& deselected);
    void on_tab_changed(int index);

private:
    void setup_ui();
    QTableView* make_table_view(QSortFilterProxyModel* proxy);
    void update_tab_counts();

    ClientManager* client_manager_;
    QString current_result_id_;

    std::unique_ptr<ClientTelemetryLogModel> model_;

    // Source-filtered proxy views
    QSortFilterProxyModel* ore_proxy_{nullptr};   ///< Source LIKE %ore_log%
    QSortFilterProxyModel* wrap_proxy_{nullptr};  ///< Source LIKE %wrapper%
    QSortFilterProxyModel* all_proxy_{nullptr};   ///< All entries

    QTabWidget* tabs_{nullptr};
    QTableView* ore_view_{nullptr};
    QTableView* wrap_view_{nullptr};
    QTableView* all_view_{nullptr};

    QSplitter* splitter_{nullptr};
    QTextEdit* detail_pane_{nullptr};
    QLabel* status_label_{nullptr};
};

} // namespace ores::qt

#endif
