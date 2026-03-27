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
#include "ores.qt/OreLogViewerWidget.hpp"

#include <chrono>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QToolBar>
#include <QAction>
#include "ores.qt/TelemetryLogDelegate.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"

namespace ores::qt {

using namespace ores::logging;

OreLogViewerWidget::OreLogViewerWidget(ClientManager* clientManager,
                                       QWidget* parent)
    : QWidget(parent),
      client_manager_(clientManager),
      model_(std::make_unique<ClientTelemetryLogModel>(clientManager)) {

    connect(model_.get(), &ClientTelemetryLogModel::dataLoaded,
            this, &OreLogViewerWidget::on_data_loaded);
    connect(model_.get(), &ClientTelemetryLogModel::loadError,
            this, &OreLogViewerWidget::on_load_error);

    setup_ui();
}

void OreLogViewerWidget::setup_ui() {
    auto* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);

    // Toolbar
    auto* toolbar = new QToolBar(this);
    toolbar->setMovable(false);
    toolbar->setToolButtonStyle(Qt::ToolButtonIconOnly);

    auto* reload_action = new QAction(
        IconUtils::createRecoloredIcon(Icon::ArrowClockwise,
            color_constants::icon_color),
        tr("Refresh"), this);
    connect(reload_action, &QAction::triggered,
            this, &OreLogViewerWidget::refresh);
    toolbar->addAction(reload_action);

    status_label_ = new QLabel(tr("No result selected"), this);
    status_label_->setContentsMargins(8, 0, 0, 0);
    toolbar->addWidget(status_label_);
    layout->addWidget(toolbar);

    // Proxy models
    const int source_col = ClientTelemetryLogModel::Source;

    ore_proxy_ = new QSortFilterProxyModel(this);
    ore_proxy_->setSourceModel(model_.get());
    ore_proxy_->setFilterKeyColumn(source_col);
    ore_proxy_->setFilterFixedString("ore_log");

    wrap_proxy_ = new QSortFilterProxyModel(this);
    wrap_proxy_->setSourceModel(model_.get());
    wrap_proxy_->setFilterKeyColumn(source_col);
    wrap_proxy_->setFilterFixedString("wrapper");

    all_proxy_ = new QSortFilterProxyModel(this);
    all_proxy_->setSourceModel(model_.get());

    // Tabs
    tabs_ = new QTabWidget(this);
    ore_view_  = make_table_view(ore_proxy_);
    wrap_view_ = make_table_view(wrap_proxy_);
    all_view_  = make_table_view(all_proxy_);

    tabs_->addTab(ore_view_,  tr("ORE Engine"));
    tabs_->addTab(wrap_view_, tr("Wrapper"));
    tabs_->addTab(all_view_,  tr("All"));

    connect(tabs_, &QTabWidget::currentChanged,
            this, &OreLogViewerWidget::on_tab_changed);

    // Detail pane
    detail_pane_ = new QTextEdit(this);
    detail_pane_->setReadOnly(true);
    detail_pane_->setMaximumHeight(120);
    detail_pane_->setPlaceholderText(tr("Select a log entry to see the full message."));
    QFont mono;
    mono.setFamily("Monospace");
    mono.setStyleHint(QFont::Monospace);
    mono.setPointSize(9);
    detail_pane_->setFont(mono);

    splitter_ = new QSplitter(Qt::Vertical, this);
    splitter_->addWidget(tabs_);
    splitter_->addWidget(detail_pane_);
    splitter_->setStretchFactor(0, 4);
    splitter_->setStretchFactor(1, 1);

    layout->addWidget(splitter_);
}

QTableView* OreLogViewerWidget::make_table_view(QSortFilterProxyModel* proxy) {
    auto* view = new QTableView(this);
    view->setModel(proxy);
    view->setItemDelegate(new TelemetryLogDelegate(view));
    view->setSelectionBehavior(QAbstractItemView::SelectRows);
    view->setSelectionMode(QAbstractItemView::SingleSelection);
    view->setEditTriggers(QAbstractItemView::NoEditTriggers);
    view->setAlternatingRowColors(true);
    view->setSortingEnabled(true);
    view->horizontalHeader()->setStretchLastSection(true);
    view->horizontalHeader()->setSectionResizeMode(
        ClientTelemetryLogModel::Message, QHeaderView::Stretch);
    view->verticalHeader()->setVisible(false);

    connect(view->selectionModel(), &QItemSelectionModel::selectionChanged,
            this, &OreLogViewerWidget::on_selection_changed);

    return view;
}

void OreLogViewerWidget::load_result(const QString& result_id) {
    BOOST_LOG_SEV(lg(), debug) << "Loading logs for result: "
                               << result_id.toStdString();
    current_result_id_ = result_id;
    model_->set_tag_filter(result_id.toStdString());
    status_label_->setText(tr("Loading…"));
    // Refresh the end_time to now so logs from jobs run after the model was
    // constructed are included in the query window.
    const auto now = std::chrono::system_clock::now();
    model_->load_logs(now - std::chrono::hours(24), now);
}

void OreLogViewerWidget::clear() {
    current_result_id_.clear();
    model_->clear();
    detail_pane_->clear();
    status_label_->setText(tr("No result selected"));
}

void OreLogViewerWidget::refresh() {
    if (current_result_id_.isEmpty()) return;
    load_result(current_result_id_);
}

void OreLogViewerWidget::on_data_loaded() {
    update_tab_counts();
    status_label_->setText(
        tr("%1 entries").arg(model_->rowCount()));
}

void OreLogViewerWidget::on_load_error(const QString& message,
                                        const QString& details) {
    BOOST_LOG_SEV(lg(), error) << "Failed to load ORE logs: "
                               << message.toStdString();
    status_label_->setText(tr("Error: %1").arg(message));
    MessageBoxHelper::critical(this, tr("Log Load Error"), message, details);
}

void OreLogViewerWidget::on_selection_changed(
    const QItemSelection& selected, const QItemSelection&) {
    if (selected.indexes().isEmpty()) {
        detail_pane_->clear();
        return;
    }

    // Identify which view triggered the signal and retrieve the source row.
    const QModelIndex proxy_idx = selected.indexes().first();
    const auto* proxy = qobject_cast<const QSortFilterProxyModel*>(
        proxy_idx.model());
    if (!proxy) return;

    const QModelIndex src_idx = proxy->mapToSource(proxy_idx);
    const auto* entry = model_->get_entry(src_idx.row());
    if (!entry) return;

    detail_pane_->setText(QString::fromStdString(entry->message));
}

void OreLogViewerWidget::on_tab_changed(int /*index*/) {
    detail_pane_->clear();
}

void OreLogViewerWidget::update_tab_counts() {
    tabs_->setTabText(0, tr("ORE Engine (%1)").arg(ore_proxy_->rowCount()));
    tabs_->setTabText(1, tr("Wrapper (%1)").arg(wrap_proxy_->rowCount()));
    tabs_->setTabText(2, tr("All (%1)").arg(all_proxy_->rowCount()));
}

} // namespace ores::qt
