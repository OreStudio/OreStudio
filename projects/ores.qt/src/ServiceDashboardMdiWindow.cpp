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
#include "ores.qt/ServiceDashboardMdiWindow.hpp"

#include <ctime>
#include <chrono>
#include <QDateTime>
#include <QHBoxLayout>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QMessageBox>
#include <QPointer>
#include <QHeaderView>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.telemetry/messaging/service_samples_protocol.hpp"
#include "ores.compute.api/messaging/telemetry_protocol.hpp"
#include "ores.controller.api/messaging/service_instance_protocol.hpp"
#include "ores.controller.api/messaging/service_definition_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;
namespace compute = ores::compute;
namespace controller_api = ores::controller::api;

namespace {

struct ServiceSamplesResult {
    bool success{false};
    QString error;
    std::vector<telemetry::domain::service_sample> samples;
    // Grid stats for wrapper node row (optional)
    bool has_grid_stats{false};
    int total_hosts{0};
    int online_hosts{0};
    std::chrono::system_clock::time_point grid_sampled_at;
    // Controller service definitions (optional — absent if controller unavailable)
    std::vector<controller_api::domain::service_definition> service_definitions;
};

struct InstanceDetailsResult {
    bool success{false};
    QString error;
    std::vector<controller_api::domain::service_instance> instances;
};

QString format_timepoint(
    std::optional<std::chrono::system_clock::time_point> tp) {
    if (!tp)
        return QStringLiteral("-");
    const auto t = std::chrono::system_clock::to_time_t(*tp);
    char buf[32];
    std::strftime(buf, sizeof(buf), "%Y-%m-%d %H:%M:%S", std::gmtime(&t));
    return QString::fromLatin1(buf);
}

}

ServiceDashboardMdiWindow::ServiceDashboardMdiWindow(
    ClientManager* clientManager,
    QWidget* parent)
    : QWidget(parent),
      clientManager_(clientManager),
      toolbar_(nullptr),
      refreshAction_(nullptr),
      autoRefreshAction_(nullptr),
      table_(nullptr),
      detailGroup_(nullptr),
      detailServiceLabel_(nullptr),
      replicasSpinBox_(nullptr),
      applyReplicasButton_(nullptr),
      detailTable_(nullptr),
      autoRefreshTimer_(nullptr) {

    autoRefreshTimer_ = new QTimer(this);
    autoRefreshTimer_->setInterval(30000);  // 30 seconds
    connect(autoRefreshTimer_, &QTimer::timeout, this,
            &ServiceDashboardMdiWindow::refresh);

    setupUi();
    refresh();
}

void ServiceDashboardMdiWindow::setupUi() {
    auto* mainLayout = new QVBoxLayout(this);
    mainLayout->setContentsMargins(4, 4, 4, 4);
    mainLayout->setSpacing(4);

    setupToolbar();
    mainLayout->addWidget(toolbar_);

    // Overview table
    table_ = new QTableWidget(this);
    table_->setColumnCount(5);
    table_->setHorizontalHeaderLabels({
        tr("Status"), tr("Service"), tr("Instance"), tr("Version"), tr("Last Seen")
    });
    table_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    table_->setSelectionBehavior(QAbstractItemView::SelectRows);
    table_->setSelectionMode(QAbstractItemView::SingleSelection);
    table_->setAlternatingRowColors(true);
    table_->horizontalHeader()->setStretchLastSection(true);
    table_->verticalHeader()->setVisible(false);
    table_->setSortingEnabled(true);
    table_->setColumnWidth(0, 70);
    connect(table_, &QTableWidget::currentCellChanged, this,
            [this](int row, int, int, int) { onRowSelected(row); });

    // Detail panel
    detailGroup_ = new QGroupBox(tr("Service Details"), this);
    detailGroup_->setVisible(false);

    auto* detailLayout = new QVBoxLayout(detailGroup_);

    detailServiceLabel_ = new QLabel(detailGroup_);
    detailLayout->addWidget(detailServiceLabel_);

    auto* replicasRow = new QHBoxLayout;
    replicasRow->addWidget(new QLabel(tr("Desired Replicas:"), detailGroup_));
    replicasSpinBox_ = new QSpinBox(detailGroup_);
    replicasSpinBox_->setRange(0, 64);
    replicasSpinBox_->setFixedWidth(80);
    replicasRow->addWidget(replicasSpinBox_);
    applyReplicasButton_ = new QPushButton(tr("Apply"), detailGroup_);
    applyReplicasButton_->setFixedWidth(80);
    replicasRow->addWidget(applyReplicasButton_);
    replicasRow->addStretch();
    detailLayout->addLayout(replicasRow);

    connect(applyReplicasButton_, &QPushButton::clicked,
            this, &ServiceDashboardMdiWindow::onApplyReplicas);

    detailTable_ = new QTableWidget(detailGroup_);
    detailTable_->setColumnCount(7);
    detailTable_->setHorizontalHeaderLabels({
        tr("Replica#"), tr("UUID"), tr("Phase"), tr("PID"),
        tr("Started At"), tr("Stopped At"), tr("Restarts")
    });
    detailTable_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    detailTable_->setSelectionBehavior(QAbstractItemView::SelectRows);
    detailTable_->setSelectionMode(QAbstractItemView::SingleSelection);
    detailTable_->setAlternatingRowColors(true);
    detailTable_->horizontalHeader()->setStretchLastSection(false);
    detailTable_->horizontalHeader()->setSectionResizeMode(
        1, QHeaderView::Stretch);
    detailTable_->verticalHeader()->setVisible(false);
    detailLayout->addWidget(detailTable_);

    // Use a splitter so the user can resize overview vs details
    auto* splitter = new QSplitter(Qt::Vertical, this);
    splitter->addWidget(table_);
    splitter->addWidget(detailGroup_);
    splitter->setStretchFactor(0, 2);
    splitter->setStretchFactor(1, 1);

    mainLayout->addWidget(splitter);
}

void ServiceDashboardMdiWindow::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    refreshAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::ArrowClockwise, IconUtils::DefaultIconColor),
        tr("Refresh"));
    refreshAction_->setToolTip(tr("Refresh service status"));
    connect(refreshAction_, &QAction::triggered,
            this, &ServiceDashboardMdiWindow::refresh);

    autoRefreshAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::ArrowSync, IconUtils::DefaultIconColor),
        tr("Auto-Refresh"));
    autoRefreshAction_->setToolTip(tr("Toggle automatic refresh every 30 seconds"));
    autoRefreshAction_->setCheckable(true);
    autoRefreshAction_->setChecked(false);
    connect(autoRefreshAction_, &QAction::toggled,
            this, &ServiceDashboardMdiWindow::onRefreshToggled);
}

void ServiceDashboardMdiWindow::refresh() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        emit statusChanged(tr("Not connected"));
        return;
    }

    refreshAction_->setEnabled(false);
    emit statusChanged(tr("Refreshing service dashboard..."));
    loadSamples();

    // Reload details for currently selected service.
    if (!selectedServiceName_.empty())
        loadInstanceDetails(QString::fromStdString(selectedServiceName_));
}

void ServiceDashboardMdiWindow::loadSamples() {
    QPointer<ServiceDashboardMdiWindow> self = this;

    QFuture<ServiceSamplesResult> future =
        QtConcurrent::run([self]() -> ServiceSamplesResult {
            if (!self || !self->clientManager_)
                return {};

            auto resp = self->clientManager_->process_authenticated_request(
                telemetry::messaging::get_service_samples_request{});

            if (!resp || !resp->success) {
                ServiceSamplesResult r;
                r.error = QString::fromStdString(
                    resp ? resp->message : "Failed to contact telemetry service");
                return r;
            }

            ServiceSamplesResult r;
            r.success = true;
            r.samples = resp->samples;

            // Fetch grid stats (best-effort).
            try {
                auto grid = self->clientManager_->process_authenticated_request(
                    compute::messaging::get_grid_stats_request{});
                if (grid && grid->success) {
                    r.has_grid_stats = true;
                    r.total_hosts    = grid->total_hosts;
                    r.online_hosts   = grid->online_hosts;
                    const auto qdt = QDateTime::fromString(
                        QString::fromStdString(grid->sampled_at), Qt::ISODate);
                    if (qdt.isValid())
                        r.grid_sampled_at = std::chrono::system_clock::from_time_t(
                            qdt.toSecsSinceEpoch());
                }
            } catch (...) {}

            // Fetch controller service definitions (best-effort).
            try {
                auto defs = self->clientManager_->process_authenticated_request(
                    controller_api::messaging::list_service_definitions_request{});
                if (defs && defs->success)
                    r.service_definitions = defs->service_definitions;
            } catch (...) {}

            return r;
        });

    auto* watcher = new QFutureWatcher<ServiceSamplesResult>(this);
    connect(watcher, &QFutureWatcher<ServiceSamplesResult>::finished, this,
            [self, watcher]() {
        const auto result = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        self->refreshAction_->setEnabled(true);

        if (!result.success) {
            const QString msg = result.error.isEmpty()
                ? tr("Failed to load service samples")
                : result.error;
            BOOST_LOG_SEV(lg(), error) << "Service dashboard load failed: "
                                       << msg.toStdString();
            emit self->errorOccurred(msg);
            return;
        }

        // Store definitions for use by the detail panel.
        self->serviceDefinitions_ = result.service_definitions;

        const auto now = std::chrono::system_clock::now();

        const int extra_rows = result.has_grid_stats ? 1 : 0;
        self->table_->setSortingEnabled(false);
        self->table_->setRowCount(
            static_cast<int>(result.samples.size()) + extra_rows);

        int row = 0;
        for (const auto& sample : result.samples) {
            using namespace std::chrono;
            const auto age_secs = duration_cast<seconds>(
                now - sample.sampled_at).count();

            QString status_text;
            QColor  status_color;
            if (age_secs < 30) {
                status_text  = tr("Green");
                status_color = color_constants::level_info;
            } else if (age_secs < 120) {
                status_text  = tr("Amber");
                status_color = color_constants::level_warn;
            } else {
                status_text  = tr("Offline");
                status_color = color_constants::level_trace;
            }

            auto makeItem = [](const QString& text) {
                auto* item = new QTableWidgetItem(text);
                item->setTextAlignment(Qt::AlignVCenter | Qt::AlignLeft);
                return item;
            };

            auto* statusItem = new QTableWidgetItem(status_text);
            statusItem->setTextAlignment(Qt::AlignCenter);
            statusItem->setForeground(status_color);
            QFont f = statusItem->font();
            f.setBold(true);
            statusItem->setFont(f);

            self->table_->setItem(row, 0, statusItem);
            self->table_->setItem(row, 1,
                makeItem(QString::fromStdString(sample.service_name)));
            self->table_->setItem(row, 2,
                makeItem(QString::fromStdString(sample.instance_id)));
            self->table_->setItem(row, 3,
                makeItem(QString::fromStdString(sample.version)));

            QString last_seen;
            if (age_secs < 60)
                last_seen = tr("%1s ago").arg(age_secs);
            else if (age_secs < 3600)
                last_seen = tr("%1m ago").arg(age_secs / 60);
            else
                last_seen = tr("%1h ago").arg(age_secs / 3600);
            self->table_->setItem(row, 4, makeItem(last_seen));

            ++row;
        }

        // Wrapper nodes row
        if (result.has_grid_stats) {
            using namespace std::chrono;
            const auto age_secs = duration_cast<seconds>(
                now - result.grid_sampled_at).count();

            QString status_text;
            QColor  status_color;
            if (result.online_hosts > 0 && age_secs < 90) {
                status_text  = tr("Green");
                status_color = color_constants::level_info;
            } else if (result.total_hosts > 0) {
                status_text  = tr("Amber");
                status_color = color_constants::level_warn;
            } else {
                status_text  = tr("Red");
                status_color = color_constants::level_error;
            }

            auto makeItem = [](const QString& text) {
                auto* item = new QTableWidgetItem(text);
                item->setTextAlignment(Qt::AlignVCenter | Qt::AlignLeft);
                return item;
            };

            auto* statusItem = new QTableWidgetItem(status_text);
            statusItem->setTextAlignment(Qt::AlignCenter);
            statusItem->setForeground(status_color);
            QFont f = statusItem->font();
            f.setBold(true);
            statusItem->setFont(f);

            const QString instance_text =
                tr("%1 node(s), %2 online")
                    .arg(result.total_hosts)
                    .arg(result.online_hosts);

            QString last_seen;
            if (age_secs < 60)
                last_seen = tr("%1s ago").arg(age_secs);
            else if (age_secs < 3600)
                last_seen = tr("%1m ago").arg(age_secs / 60);
            else
                last_seen = tr("%1h ago").arg(age_secs / 3600);

            self->table_->setItem(row, 0, statusItem);
            self->table_->setItem(row, 1, makeItem(tr("ores.compute.wrapper")));
            self->table_->setItem(row, 2, makeItem(instance_text));
            self->table_->setItem(row, 3, makeItem(tr("-")));
            self->table_->setItem(row, 4, makeItem(last_seen));
        }

        self->table_->setSortingEnabled(true);
        self->table_->resizeColumnsToContents();
        self->table_->horizontalHeader()->setStretchLastSection(true);

        BOOST_LOG_SEV(lg(), debug) << "Service dashboard: "
                                   << result.samples.size() << " services";
        emit self->statusChanged(
            tr("Service dashboard updated: %1 service instance(s)")
                .arg(result.samples.size()));
    });
    watcher->setFuture(future);
}

void ServiceDashboardMdiWindow::onRowSelected(int row) {
    if (row < 0 || row >= table_->rowCount()) {
        detailGroup_->setVisible(false);
        selectedServiceName_.clear();
        return;
    }

    const auto* item = table_->item(row, 1);
    if (!item) {
        detailGroup_->setVisible(false);
        selectedServiceName_.clear();
        return;
    }

    const QString serviceName = item->text();
    selectedServiceName_ = serviceName.toStdString();

    detailServiceLabel_->setText(tr("Service: <b>%1</b>").arg(serviceName));

    // Populate desired replicas spinbox from loaded definitions.
    int desiredReplicas = 1;
    for (const auto& def : serviceDefinitions_) {
        if (def.service_name == selectedServiceName_) {
            desiredReplicas = def.desired_replicas;
            break;
        }
    }
    replicasSpinBox_->setValue(desiredReplicas);

    detailGroup_->setVisible(true);
    loadInstanceDetails(serviceName);
}

void ServiceDashboardMdiWindow::loadInstanceDetails(const QString& serviceName) {
    if (!clientManager_ || !clientManager_->isConnected())
        return;

    QPointer<ServiceDashboardMdiWindow> self = this;
    const std::string svcName = serviceName.toStdString();

    QFuture<InstanceDetailsResult> future =
        QtConcurrent::run([self, svcName]() -> InstanceDetailsResult {
            if (!self || !self->clientManager_)
                return {};

            controller_api::messaging::list_service_instances_request req;
            req.service_name = svcName;

            auto resp = self->clientManager_->process_authenticated_request(req);
            if (!resp || !resp->success) {
                InstanceDetailsResult r;
                r.error = QString::fromStdString(
                    resp ? resp->message
                         : "Failed to contact controller service");
                return r;
            }

            InstanceDetailsResult r;
            r.success = true;
            r.instances = resp->service_instances;
            return r;
        });

    auto* watcher = new QFutureWatcher<InstanceDetailsResult>(this);
    connect(watcher, &QFutureWatcher<InstanceDetailsResult>::finished, this,
            [self, watcher, serviceName]() {
        const auto result = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        // Ignore if the user selected a different service while loading.
        if (serviceName.toStdString() != self->selectedServiceName_) return;

        if (!result.success) {
            BOOST_LOG_SEV(lg(), warn)
                << "Could not load instance details for "
                << serviceName.toStdString() << ": "
                << result.error.toStdString();
            self->detailTable_->setRowCount(0);
            return;
        }

        self->detailTable_->setSortingEnabled(false);
        self->detailTable_->setRowCount(
            static_cast<int>(result.instances.size()));

        int row = 0;
        for (const auto& inst : result.instances) {
            auto makeItem = [](const QString& text) {
                auto* item = new QTableWidgetItem(text);
                item->setTextAlignment(Qt::AlignVCenter | Qt::AlignLeft);
                return item;
            };

            self->detailTable_->setItem(row, 0,
                makeItem(QString::number(inst.replica_index)));
            self->detailTable_->setItem(row, 1,
                makeItem(QString::fromStdString(
                    boost::uuids::to_string(inst.id))));
            self->detailTable_->setItem(row, 2,
                makeItem(QString::fromStdString(inst.phase)));
            self->detailTable_->setItem(row, 3,
                makeItem(inst.pid ? QString::number(*inst.pid)
                                  : QStringLiteral("-")));
            self->detailTable_->setItem(row, 4,
                makeItem(format_timepoint(inst.started_at)));
            self->detailTable_->setItem(row, 5,
                makeItem(format_timepoint(inst.stopped_at)));
            self->detailTable_->setItem(row, 6,
                makeItem(QString::number(inst.restart_count)));
            ++row;
        }

        self->detailTable_->setSortingEnabled(true);
        self->detailTable_->resizeColumnsToContents();
        self->detailTable_->horizontalHeader()->setSectionResizeMode(
            1, QHeaderView::Stretch);
    });
    watcher->setFuture(future);
}

void ServiceDashboardMdiWindow::onApplyReplicas() {
    if (selectedServiceName_.empty() || !clientManager_ ||
            !clientManager_->isConnected())
        return;

    // Find the matching definition.
    controller_api::domain::service_definition def;
    bool found = false;
    for (const auto& d : serviceDefinitions_) {
        if (d.service_name == selectedServiceName_) {
            def = d;
            found = true;
            break;
        }
    }

    if (!found) {
        BOOST_LOG_SEV(lg(), warn)
            << "No service definition found for: " << selectedServiceName_;
        QMessageBox::warning(this, tr("Apply Replicas"),
            tr("No service definition found for '%1'. "
               "Ensure the controller service is running.")
                .arg(QString::fromStdString(selectedServiceName_)));
        return;
    }

    def.desired_replicas = replicasSpinBox_->value();

    QPointer<ServiceDashboardMdiWindow> self = this;
    const std::string svcName = selectedServiceName_;
    const int newReplicas = def.desired_replicas;

    QFuture<bool> future = QtConcurrent::run(
            [self, def, svcName]() -> bool {
        if (!self || !self->clientManager_)
            return false;

        controller_api::messaging::save_service_definition_request req;
        req.service_definition = def;
        req.change_reason_code = "MANUAL";
        req.change_commentary = "Set via service dashboard";

        auto resp = self->clientManager_->process_authenticated_request(req);
        return resp && resp->success;
    });

    auto* watcher = new QFutureWatcher<bool>(this);
    connect(watcher, &QFutureWatcher<bool>::finished, this,
            [self, watcher, svcName, newReplicas]() {
        const bool ok = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        if (ok) {
            BOOST_LOG_SEV(lg(), info)
                << "Updated desired_replicas=" << newReplicas
                << " for " << svcName;
            emit self->statusChanged(
                tr("Updated desired replicas for '%1' to %2.")
                    .arg(QString::fromStdString(svcName))
                    .arg(newReplicas));
            // Refresh definitions so the spinbox stays in sync.
            self->loadSamples();
        } else {
            BOOST_LOG_SEV(lg(), error)
                << "Failed to update replicas for " << svcName;
            QMessageBox::critical(self, tr("Apply Replicas"),
                tr("Failed to update desired replicas for '%1'.")
                    .arg(QString::fromStdString(svcName)));
        }
    });
    watcher->setFuture(future);
}

void ServiceDashboardMdiWindow::onRefreshToggled(bool checked) {
    if (checked) {
        BOOST_LOG_SEV(lg(), info) << "Auto-refresh enabled for service dashboard";
        autoRefreshTimer_->start();
    } else {
        BOOST_LOG_SEV(lg(), info) << "Auto-refresh disabled for service dashboard";
        autoRefreshTimer_->stop();
    }
}

}
