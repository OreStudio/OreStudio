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
#include "ores.qt/SchedulerMonitorMdiWindow.hpp"

#include <QHeaderView>
#include <QLabel>
#include <QPointer>
#include <QtConcurrent>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"

namespace ores::qt {

using namespace ores::logging;

SchedulerMonitorMdiWindow::SchedulerMonitorMdiWindow(
    ClientManager* clientManager,
    QWidget* parent)
    : QWidget(parent),
      clientManager_(clientManager),
      toolbar_(nullptr),
      refreshAction_(nullptr),
      autoRefreshAction_(nullptr),
      intervalSpin_(nullptr),
      table_(nullptr),
      autoRefreshTimer_(new QTimer(this)) {

    setupUi();

    autoRefreshTimer_->setInterval(15000); // default 15 s
    connect(autoRefreshTimer_, &QTimer::timeout, this, &SchedulerMonitorMdiWindow::refresh);

    refresh();
}

void SchedulerMonitorMdiWindow::setupUi() {
    auto* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);

    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    refreshAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::ArrowClockwise, IconUtils::DefaultIconColor),
        tr("Refresh"));
    connect(refreshAction_, &QAction::triggered, this, &SchedulerMonitorMdiWindow::refresh);

    toolbar_->addSeparator();

    autoRefreshAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Clock, IconUtils::DefaultIconColor),
        tr("Auto Refresh"));
    autoRefreshAction_->setCheckable(true);
    autoRefreshAction_->setChecked(false);
    connect(autoRefreshAction_, &QAction::toggled,
            this, &SchedulerMonitorMdiWindow::onAutoRefreshToggled);

    toolbar_->addWidget(new QLabel(tr(" Every "), this));

    intervalSpin_ = new QSpinBox(this);
    intervalSpin_->setRange(5, 3600);
    intervalSpin_->setValue(15);
    intervalSpin_->setSuffix(tr(" s"));
    intervalSpin_->setToolTip(tr("Auto-refresh interval in seconds"));
    connect(intervalSpin_, &QSpinBox::valueChanged,
            this, &SchedulerMonitorMdiWindow::onAutoRefreshIntervalChanged);
    toolbar_->addWidget(intervalSpin_);

    layout->addWidget(toolbar_);

    table_ = new QTableWidget(0, ColCount, this);
    table_->setHorizontalHeaderLabels({
        tr("Job Name"), tr("Schedule"), tr("Active"),
        tr("Last Run"), tr("Last Status"), tr("Next Fire"), tr("Running")});
    table_->setSelectionBehavior(QAbstractItemView::SelectRows);
    table_->setSelectionMode(QAbstractItemView::SingleSelection);
    table_->setAlternatingRowColors(true);
    table_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    table_->verticalHeader()->setVisible(false);
    table_->horizontalHeader()->setStretchLastSection(true);
    table_->setSortingEnabled(true);

    layout->addWidget(table_);
}

void SchedulerMonitorMdiWindow::onAutoRefreshToggled(bool enabled) {
    if (enabled)
        autoRefreshTimer_->start();
    else
        autoRefreshTimer_->stop();
}

void SchedulerMonitorMdiWindow::onAutoRefreshIntervalChanged(int seconds) {
    autoRefreshTimer_->setInterval(seconds * 1000);
}

void SchedulerMonitorMdiWindow::refresh() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        emit errorOccurred(tr("Not connected to server"));
        return;
    }

    emit statusChanged(tr("Loading scheduler status..."));

    QPointer<SchedulerMonitorMdiWindow> self = this;

    struct FetchResult {
        scheduler::messaging::get_scheduler_status_response response;
        QString error;
    };

    auto future = QtConcurrent::run([self]() -> FetchResult {
        if (!self || !self->clientManager_)
            return {.error = tr("Widget destroyed during fetch")};
        scheduler::messaging::get_scheduler_status_request req;
        auto result = self->clientManager_->process_authenticated_request(std::move(req));
        if (!result)
            return {.error = QString::fromStdString(result.error())};
        if (!result->success)
            return {.error = QString::fromStdString(result->message)};
        return {.response = std::move(*result)};
    });

    auto* watcher = new QFutureWatcher<FetchResult>(this);
    connect(watcher, &QFutureWatcher<FetchResult>::finished,
            this, [self, watcher]() {
        watcher->deleteLater();
        if (!self) return;

        const auto res = watcher->result();
        if (!res.error.isEmpty()) {
            BOOST_LOG_SEV(lg(), error) << "Scheduler status fetch failed: "
                                       << res.error.toStdString();
            emit self->errorOccurred(res.error);
            return;
        }

        self->applyStatus(res.response.jobs);

        const int active = res.response.total_active;
        const int running = res.response.total_running;
        emit self->statusChanged(
            tr("Scheduler: %1 active job(s), %2 currently running")
                .arg(active).arg(running));
    });

    watcher->setFuture(future);
}

void SchedulerMonitorMdiWindow::applyStatus(
    const std::vector<scheduler::messaging::job_schedule_status>& jobs) {

    table_->setSortingEnabled(false);
    table_->setRowCount(static_cast<int>(jobs.size()));

    auto cell = [this](int row, int col, const QString& text) {
        auto* item = table_->item(row, col);
        if (!item) {
            item = new QTableWidgetItem(text);
            item->setFlags(item->flags() & ~Qt::ItemIsEditable);
            table_->setItem(row, col, item);
        } else {
            item->setText(text);
        }
    };

    for (int r = 0; r < static_cast<int>(jobs.size()); ++r) {
        const auto& j = jobs[static_cast<std::size_t>(r)];

        cell(r, ColJobName,    QString::fromStdString(j.job_name));
        cell(r, ColSchedule,   QString::fromStdString(j.schedule_expression));
        cell(r, ColActive,     j.is_active ? tr("Active") : tr("Paused"));
        cell(r, ColLastRun,    j.last_run_at
            ? QString::fromStdString(*j.last_run_at) : tr("Never"));
        cell(r, ColLastStatus, j.last_run_status
            ? QString::fromStdString(*j.last_run_status) : tr("—"));
        cell(r, ColNextFire,   (j.is_active && j.next_fire_at)
            ? QString::fromStdString(*j.next_fire_at) : tr("—"));
        cell(r, ColRunning,    j.running_count > 0
            ? QString::number(j.running_count) : tr("—"));

        // Colour last-status cell
        if (auto* statusItem = table_->item(r, ColLastStatus)) {
            if (j.last_run_status) {
                if (*j.last_run_status == "failed")
                    statusItem->setForeground(Qt::red);
                else if (*j.last_run_status == "succeeded")
                    statusItem->setForeground(Qt::darkGreen);
                else
                    statusItem->setForeground(Qt::darkYellow);
            }
        }
    }

    table_->setSortingEnabled(true);
    table_->resizeColumnsToContents();
}

}
