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

#include <climits>
#include <chrono>
#include "ores.platform/time/datetime.hpp"
#include <QPainter>
#include <QDateTime>
#include <QHBoxLayout>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QMessageBox>
#include <QPointer>
#include <QHeaderView>
#include <QApplication>
#include <QStyledItemDelegate>
#include <QStyleOptionViewItem>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/DelegatePaintUtils.hpp"
#include "ores.qt/FontUtils.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.telemetry/messaging/service_samples_protocol.hpp"
#include "ores.controller.api/messaging/service_instance_protocol.hpp"
#include "ores.controller.api/messaging/service_definition_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;
namespace controller_api = ores::controller::api;

// ─────────────────────────────────────────────────────────────────────────────
// Local types
// ─────────────────────────────────────────────────────────────────────────────

namespace {

// Main overview table columns
enum class Col { Status = 0, Service, Description, Replicas, Version, LastSeen };
// Instance detail table columns
enum class DCol { Replica = 0, Phase, Pid, StartedAt, StoppedAt, Restarts, LastError, Uuid };

// Per-service aggregated data built from heartbeat samples + service definitions.
struct ServiceRow {
    std::string service_name;
    std::string description;
    int desired_replicas = 1;
    int online_replicas    = 0;   // samples with age < 120 s (telemetry heartbeat)
    int running_replicas   = 0;   // instances with phase == "running" (controller DB)
    int pending_replicas   = 0;   // instances with phase == "pending" (not yet launched)
    int failed_replicas    = 0;   // instances with phase == "failed" (exceeded max_restart_count)
    int restarting_replicas = 0;  // instances with restart_count > 0 that are not failed
    std::string version;          // from most-recent sample
    long long   last_seen_secs = LLONG_MAX; // age in seconds, LLONG_MAX = never seen
};

struct SamplesResult {
    bool success{false};
    QString error;
    std::vector<ServiceRow> rows;
    std::vector<controller_api::domain::service_definition> service_definitions;
};

struct InstancesResult {
    bool success{false};
    QString error;
    std::vector<controller_api::domain::service_instance> instances;
};

struct ServiceActionResult {
    bool success{false};
    QString error;
};

// ─────────────────────────────────────────────────────────────────────────────
// Helpers
// ─────────────────────────────────────────────────────────────────────────────

QString relative_time(long long secs) {
    if (secs == LLONG_MAX) return QStringLiteral("Never");
    if (secs < 60)   return QStringLiteral("%1s ago").arg(secs);
    if (secs < 3600) return QStringLiteral("%1m ago").arg(secs / 60);
    return QStringLiteral("%1h ago").arg(secs / 3600);
}

QString format_timepoint(
    std::optional<std::chrono::system_clock::time_point> tp) {
    if (!tp) return QStringLiteral("-");
    return QString::fromStdString(
        ores::platform::time::datetime::to_iso8601_utc(*tp));
}

std::pair<QString, QColor> status_for_row(const ServiceRow& r) {
    // Failed: all replicas have hit max_restart_count — needs operator action.
    if (r.failed_replicas > 0 && r.running_replicas == 0 && r.online_replicas == 0)
        return { QStringLiteral("Failed"), color_constants::level_error };

    // Starting: replicas exist but none are running yet (first launch or pending).
    if (r.pending_replicas > 0 && r.running_replicas == 0 && r.online_replicas == 0)
        return { QStringLiteral("Starting"), color_constants::level_debug };

    // Restarting: replicas have crashed and are being retried, none currently up.
    if (r.restarting_replicas > 0 && r.running_replicas == 0 && r.online_replicas == 0)
        return { QStringLiteral("Restarting"), color_constants::level_warn };

    // Services with NATS telemetry heartbeats: use heartbeat-based status.
    if (r.last_seen_secs != LLONG_MAX) {
        if (r.online_replicas == 0)
            return { QStringLiteral("Offline"), color_constants::level_trace };
        if (r.online_replicas < r.desired_replicas || r.last_seen_secs >= 30)
            return { QStringLiteral("Degraded"), color_constants::level_warn };
        return { QStringLiteral("Online"), color_constants::level_info };
    }
    // No heartbeat telemetry (HTTP server, WT, compute wrappers): fall back to
    // controller instance phase.
    if (r.running_replicas == 0)
        return { QStringLiteral("Offline"), color_constants::level_trace };
    if (r.online_replicas < r.desired_replicas || r.last_seen_secs >= 30)
        return { QStringLiteral("Degraded"), color_constants::level_warn };
    return { QStringLiteral("Online"), color_constants::level_info };
}

QColor phase_color(const QString& phase) {
    if (phase == "running")  return color_constants::level_info;
    if (phase == "failed")   return color_constants::level_error;
    if (phase == "starting") return color_constants::level_debug;
    return color_constants::level_trace; // stopped, unknown
}

// ─────────────────────────────────────────────────────────────────────────────
// Badge delegate — used for both the main status column and the phase column
// in the detail table. The column that needs badge rendering is identified by
// the Qt::UserRole data being the string "badge".
// ─────────────────────────────────────────────────────────────────────────────

class BadgeDelegate final : public QStyledItemDelegate {
public:
    explicit BadgeDelegate(QObject* parent = nullptr)
        : QStyledItemDelegate(parent) {}

    void paint(QPainter* painter, const QStyleOptionViewItem& option,
               const QModelIndex& index) const override {
        if (index.data(Qt::UserRole).toString() != QStringLiteral("badge")) {
            QStyledItemDelegate::paint(painter, option, index);
            return;
        }

        QStyleOptionViewItem opt = option;
        initStyleOption(&opt, index);
        QApplication::style()->drawPrimitive(
            QStyle::PE_PanelItemViewItem, &opt, painter);

        const QString text   = index.data(Qt::DisplayRole).toString();
        const QColor  bg     = index.data(Qt::UserRole + 1).value<QColor>();
        const QColor  fg     = color_constants::level_text;

        QFont badgeFont = opt.font;
        badgeFont.setPointSize(qRound(badgeFont.pointSize() * 0.8));
        badgeFont.setBold(true);

        DelegatePaintUtils::draw_centered_badge(
            painter, opt.rect, text, bg, fg, badgeFont);
    }

    QSize sizeHint(const QStyleOptionViewItem& option,
                   const QModelIndex& index) const override {
        QSize s = QStyledItemDelegate::sizeHint(option, index);
        if (index.data(Qt::UserRole).toString() == QStringLiteral("badge"))
            s = QSize(qMax(s.width(), 80), qMax(s.height(), 24));
        return s;
    }
};

// ─────────────────────────────────────────────────────────────────────────────
// Helper: make a badge QTableWidgetItem (tagged with UserRole = "badge")
// ─────────────────────────────────────────────────────────────────────────────

QTableWidgetItem* make_badge_item(const QString& text, const QColor& bg) {
    auto* item = new QTableWidgetItem(text);
    item->setData(Qt::UserRole,     QStringLiteral("badge"));
    item->setData(Qt::UserRole + 1, bg); // badge color — NOT BackgroundRole (avoids coloring whole cell)
    item->setFlags(item->flags() & ~Qt::ItemIsEditable);
    return item;
}

QTableWidgetItem* make_item(const QString& text) {
    auto* item = new QTableWidgetItem(text);
    item->setTextAlignment(Qt::AlignVCenter | Qt::AlignLeft);
    item->setFlags(item->flags() & ~Qt::ItemIsEditable);
    return item;
}

} // namespace

// ─────────────────────────────────────────────────────────────────────────────
// ServiceDashboardMdiWindow
// ─────────────────────────────────────────────────────────────────────────────

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
      stopButton_(nullptr),
      restartButton_(nullptr),
      detailTable_(nullptr),
      autoRefreshTimer_(nullptr) {

    autoRefreshTimer_ = new QTimer(this);
    autoRefreshTimer_->setInterval(30000);
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

    // ── Overview table ────────────────────────────────────────────────────
    table_ = new QTableWidget(this);
    table_->setColumnCount(6);
    table_->setHorizontalHeaderLabels({
        tr("Status"), tr("Service"), tr("Description"),
        tr("Replicas"), tr("Version"), tr("Last Seen")
    });
    table_->setItemDelegate(new BadgeDelegate(table_));
    table_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    table_->setSelectionBehavior(QAbstractItemView::SelectRows);
    table_->setSelectionMode(QAbstractItemView::SingleSelection);
    table_->setAlternatingRowColors(true);
    table_->horizontalHeader()->setStretchLastSection(false);
    table_->horizontalHeader()->setSectionResizeMode(
        static_cast<int>(Col::Description), QHeaderView::Stretch);
    table_->verticalHeader()->setVisible(false);
    table_->setSortingEnabled(true);
    table_->setColumnWidth(static_cast<int>(Col::Status), 80);
    connect(table_, &QTableWidget::currentCellChanged, this,
            [this](int row, int, int, int) { onRowSelected(row); });

    // ── Detail panel ──────────────────────────────────────────────────────
    detailGroup_ = new QGroupBox(tr("Service Details"), this);
    detailGroup_->setVisible(false);

    auto* detailLayout = new QVBoxLayout(detailGroup_);
    detailLayout->setSpacing(4);

    detailServiceLabel_ = new QLabel(detailGroup_);
    detailLayout->addWidget(detailServiceLabel_);

    // Controls row: replicas + stop/restart
    auto* ctrlRow = new QHBoxLayout;
    ctrlRow->addWidget(new QLabel(tr("Desired Replicas:"), detailGroup_));
    replicasSpinBox_ = new QSpinBox(detailGroup_);
    replicasSpinBox_->setRange(0, 64);
    replicasSpinBox_->setFixedWidth(70);
    ctrlRow->addWidget(replicasSpinBox_);
    applyReplicasButton_ = new QPushButton(tr("Apply"), detailGroup_);
    applyReplicasButton_->setFixedWidth(70);
    ctrlRow->addWidget(applyReplicasButton_);

    ctrlRow->addSpacing(16);

    stopButton_ = new QPushButton(
        IconUtils::createRecoloredIcon(Icon::Delete, color_constants::level_error),
        tr("Stop All"), detailGroup_);
    stopButton_->setFixedWidth(90);
    ctrlRow->addWidget(stopButton_);

    restartButton_ = new QPushButton(
        IconUtils::createRecoloredIcon(Icon::ArrowClockwise, IconUtils::DefaultIconColor),
        tr("Restart All"), detailGroup_);
    restartButton_->setFixedWidth(100);
    ctrlRow->addWidget(restartButton_);

    ctrlRow->addStretch();
    detailLayout->addLayout(ctrlRow);

    connect(applyReplicasButton_, &QPushButton::clicked,
            this, &ServiceDashboardMdiWindow::onApplyReplicas);
    connect(stopButton_,    &QPushButton::clicked,
            this, &ServiceDashboardMdiWindow::onStopService);
    connect(restartButton_, &QPushButton::clicked,
            this, &ServiceDashboardMdiWindow::onRestartService);

    // Instance table
    detailTable_ = new QTableWidget(detailGroup_);
    detailTable_->setColumnCount(8);
    detailTable_->setHorizontalHeaderLabels({
        tr("Replica"), tr("Phase"), tr("PID"),
        tr("Started At"), tr("Stopped At"), tr("Restarts"), tr("Last Error"), tr("UUID")
    });
    detailTable_->setItemDelegate(new BadgeDelegate(detailTable_));
    detailTable_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    detailTable_->setSelectionBehavior(QAbstractItemView::SelectRows);
    detailTable_->setSelectionMode(QAbstractItemView::SingleSelection);
    detailTable_->setAlternatingRowColors(true);
    detailTable_->horizontalHeader()->setStretchLastSection(false);
    detailTable_->horizontalHeader()->setSectionResizeMode(
        static_cast<int>(DCol::LastError), QHeaderView::Stretch);
    detailTable_->verticalHeader()->setVisible(false);
    detailLayout->addWidget(detailTable_);

    // Splitter
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
        IconUtils::createRecoloredIcon(Icon::ArrowClockwise, IconUtils::DefaultIconColor),
        tr("Refresh"));
    refreshAction_->setToolTip(tr("Refresh service status"));
    connect(refreshAction_, &QAction::triggered,
            this, &ServiceDashboardMdiWindow::refresh);

    autoRefreshAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::ArrowSync, IconUtils::DefaultIconColor),
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
    if (!selectedServiceName_.empty())
        loadInstanceDetails(QString::fromStdString(selectedServiceName_));
}

void ServiceDashboardMdiWindow::loadSamples() {
    QPointer<ServiceDashboardMdiWindow> self = this;

    QFuture<SamplesResult> future =
        QtConcurrent::run([self]() -> SamplesResult {
            if (!self || !self->clientManager_)
                return {};

            // 1. Fetch service definitions (required — drives the row list).
            std::vector<controller_api::domain::service_definition> defs;
            try {
                auto dr = self->clientManager_->process_authenticated_request(
                    controller_api::messaging::list_service_definitions_request{});
                if (dr && dr->success)
                    defs = dr->service_definitions;
            } catch (...) {}

            if (defs.empty()) {
                SamplesResult r;
                r.error = QStringLiteral(
                    "Could not load service definitions from controller");
                return r;
            }

            // 2. Fetch heartbeat samples (best-effort).
            std::vector<telemetry::domain::service_sample> samples;
            try {
                auto sr = self->clientManager_->process_authenticated_request(
                    telemetry::messaging::get_service_samples_request{});
                if (sr && sr->success) {
                    samples = sr->samples;
                    BOOST_LOG_SEV(lg(), debug)
                        << "Service samples fetched: " << samples.size();
                } else if (!sr) {
                    BOOST_LOG_SEV(lg(), warn)
                        << "Service samples request failed: " << sr.error();
                } else {
                    BOOST_LOG_SEV(lg(), warn)
                        << "Service samples returned success=false: " << sr->message;
                }
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), warn)
                    << "Service samples exception: " << e.what();
            } catch (...) {
                BOOST_LOG_SEV(lg(), warn) << "Service samples unknown exception";
            }

            // 2b. Fetch all service instances (best-effort) for phase-based status.
            std::vector<controller_api::domain::service_instance> all_instances;
            try {
                controller_api::messaging::list_service_instances_request inst_req;
                // empty service_name returns all instances
                auto ir = self->clientManager_->process_authenticated_request(inst_req);
                if (ir && ir->success)
                    all_instances = ir->service_instances;
            } catch (...) {}

            // 3. Aggregate: one ServiceRow per definition.
            const auto now = std::chrono::system_clock::now();
            SamplesResult r;
            r.success = true;
            r.service_definitions = defs;

            if (!samples.empty()) {
                // Log one sample's sampled_at and age for timezone diagnosis.
                const auto& first = samples.front();
                const long long first_age =
                    std::chrono::duration_cast<std::chrono::seconds>(
                        now - first.sampled_at).count();
                BOOST_LOG_SEV(lg(), debug)
                    << "First sample: name=" << first.service_name
                    << " age_secs=" << first_age;
            }

            for (const auto& def : defs) {
                ServiceRow row;
                row.service_name     = def.service_name;
                row.description      = def.description.value_or(std::string{});
                row.desired_replicas = def.desired_replicas;

                for (const auto& s : samples) {
                    if (s.service_name != def.service_name) continue;
                    using namespace std::chrono;
                    const long long age =
                        duration_cast<seconds>(now - s.sampled_at).count();
                    if (age < row.last_seen_secs) {
                        row.last_seen_secs = age;
                        row.version        = s.version;
                    }
                    if (age < 120) ++row.online_replicas;
                }

                for (const auto& inst : all_instances) {
                    if (inst.service_name != def.service_name) continue;
                    if (inst.phase == "running") ++row.running_replicas;
                    else if (inst.phase == "pending") ++row.pending_replicas;
                    else if (inst.phase == "failed") ++row.failed_replicas;
                    else if (inst.restart_count > 0) ++row.restarting_replicas;
                }

                r.rows.push_back(std::move(row));
            }
            return r;
        });

    auto* watcher = new QFutureWatcher<SamplesResult>(this);
    connect(watcher, &QFutureWatcher<SamplesResult>::finished, this,
            [self, watcher]() {
        const auto result = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        self->refreshAction_->setEnabled(true);

        if (!result.success) {
            const QString msg = result.error.isEmpty()
                ? tr("Failed to load service dashboard")
                : result.error;
            BOOST_LOG_SEV(lg(), error) << msg.toStdString();
            emit self->errorOccurred(msg);
            return;
        }

        // Update definitions cache for the detail panel (replicas spinbox).
        self->serviceDefinitions_ = result.service_definitions;

        self->table_->setSortingEnabled(false);
        self->table_->setRowCount(static_cast<int>(result.rows.size()));

        int row = 0;
        for (const auto& sr : result.rows) {
            auto [status_text, status_color] = status_for_row(sr);

            self->table_->setItem(row, static_cast<int>(Col::Status),
                make_badge_item(status_text, status_color));
            self->table_->setItem(row, static_cast<int>(Col::Service),
                make_item(QString::fromStdString(sr.service_name)));
            self->table_->setItem(row, static_cast<int>(Col::Description),
                make_item(QString::fromStdString(sr.description)));

            const int display_replicas = sr.online_replicas;
            const QString replicas =
                tr("%1 / %2").arg(display_replicas).arg(sr.desired_replicas);
            self->table_->setItem(row, static_cast<int>(Col::Replicas),
                make_item(replicas));

            const QString ver = sr.version.empty()
                ? QStringLiteral("-")
                : QString::fromStdString(sr.version);
            self->table_->setItem(row, static_cast<int>(Col::Version),
                make_item(ver));
            self->table_->setItem(row, static_cast<int>(Col::LastSeen),
                make_item(relative_time(sr.last_seen_secs)));
            ++row;
        }

        self->table_->setSortingEnabled(true);
        self->table_->resizeColumnsToContents();
        self->table_->horizontalHeader()->setSectionResizeMode(
            static_cast<int>(Col::Description), QHeaderView::Stretch);
        self->table_->setColumnWidth(static_cast<int>(Col::Status), 80);

        BOOST_LOG_SEV(lg(), debug) << "Service dashboard: "
                                   << result.rows.size() << " services";
        emit self->statusChanged(
            tr("Service dashboard updated: %1 service(s)")
                .arg(result.rows.size()));
    });
    watcher->setFuture(future);
}

void ServiceDashboardMdiWindow::onRowSelected(int row) {
    if (row < 0 || row >= table_->rowCount()) {
        detailGroup_->setVisible(false);
        selectedServiceName_.clear();
        return;
    }
    const auto* item = table_->item(row, static_cast<int>(Col::Service));
    if (!item) {
        detailGroup_->setVisible(false);
        selectedServiceName_.clear();
        return;
    }

    const QString serviceName = item->text();
    selectedServiceName_ = serviceName.toStdString();
    detailServiceLabel_->setText(
        tr("Service: <b>%1</b>").arg(serviceName));

    // Populate replicas spinbox from the definition cache.
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
    if (!clientManager_ || !clientManager_->isConnected()) return;

    QPointer<ServiceDashboardMdiWindow> self = this;
    const std::string svcName = serviceName.toStdString();

    QFuture<InstancesResult> future =
        QtConcurrent::run([self, svcName]() -> InstancesResult {
            if (!self || !self->clientManager_) return {};

            controller_api::messaging::list_service_instances_request req;
            req.service_name = svcName;
            auto resp = self->clientManager_->process_authenticated_request(req);
            if (!resp || !resp->success) {
                InstancesResult r;
                r.error = QString::fromStdString(
                    resp ? resp->message : "Failed to contact controller");
                return r;
            }
            InstancesResult r;
            r.success   = true;
            r.instances = resp->service_instances;
            return r;
        });

    auto* watcher = new QFutureWatcher<InstancesResult>(this);
    connect(watcher, &QFutureWatcher<InstancesResult>::finished, this,
            [self, watcher, serviceName]() {
        const auto result = watcher->result();
        watcher->deleteLater();
        if (!self) return;
        if (serviceName.toStdString() != self->selectedServiceName_) return;

        if (!result.success) {
            BOOST_LOG_SEV(lg(), warn)
                << "Could not load instances for "
                << serviceName.toStdString();
            self->detailTable_->setRowCount(0);
            return;
        }

        self->detailTable_->setSortingEnabled(false);
        self->detailTable_->setRowCount(
            static_cast<int>(result.instances.size()));

        int row = 0;
        for (const auto& inst : result.instances) {
            const QString phase =
                QString::fromStdString(inst.phase);

            self->detailTable_->setItem(row, static_cast<int>(DCol::Replica),
                make_item(QString::number(inst.replica_index)));
            self->detailTable_->setItem(row, static_cast<int>(DCol::Phase),
                make_badge_item(phase, phase_color(phase)));
            self->detailTable_->setItem(row, static_cast<int>(DCol::Pid),
                make_item(inst.pid ? QString::number(*inst.pid)
                                   : QStringLiteral("-")));
            self->detailTable_->setItem(row, static_cast<int>(DCol::StartedAt),
                make_item(format_timepoint(inst.started_at)));
            self->detailTable_->setItem(row, static_cast<int>(DCol::StoppedAt),
                make_item(format_timepoint(inst.stopped_at)));
            self->detailTable_->setItem(row, static_cast<int>(DCol::Restarts),
                make_item(QString::number(inst.restart_count)));
            self->detailTable_->setItem(row, static_cast<int>(DCol::LastError),
                make_item(inst.last_error
                    ? QString::fromStdString(*inst.last_error)
                    : QStringLiteral("-")));
            self->detailTable_->setItem(row, static_cast<int>(DCol::Uuid),
                make_item(QString::fromStdString(
                    boost::uuids::to_string(inst.id))));
            ++row;
        }

        self->detailTable_->setSortingEnabled(true);
        self->detailTable_->resizeColumnsToContents();
        self->detailTable_->horizontalHeader()->setSectionResizeMode(
            static_cast<int>(DCol::LastError), QHeaderView::Stretch);
        self->detailTable_->horizontalHeader()->setSectionResizeMode(
            static_cast<int>(DCol::Uuid), QHeaderView::ResizeToContents);
    });
    watcher->setFuture(future);
}

void ServiceDashboardMdiWindow::onApplyReplicas() {
    if (selectedServiceName_.empty() || !clientManager_ ||
            !clientManager_->isConnected())
        return;

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
        QMessageBox::warning(this, tr("Apply Replicas"),
            tr("No definition found for '%1'. Is the controller running?")
                .arg(QString::fromStdString(selectedServiceName_)));
        return;
    }

    def.desired_replicas = replicasSpinBox_->value();

    QPointer<ServiceDashboardMdiWindow> self = this;
    const std::string svcName     = selectedServiceName_;
    const int         newReplicas = def.desired_replicas;

    QFuture<bool> future = QtConcurrent::run([self, def]() -> bool {
        if (!self || !self->clientManager_) return false;
        controller_api::messaging::save_service_definition_request req;
        req.service_definition = def;
        req.change_reason_code = "MANUAL";
        req.change_commentary  = "Set via service dashboard";
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
            emit self->statusChanged(
                tr("Updated desired replicas for '%1' to %2.")
                    .arg(QString::fromStdString(svcName)).arg(newReplicas));
            self->loadSamples();
        } else {
            QMessageBox::critical(self, tr("Apply Replicas"),
                tr("Failed to update desired replicas for '%1'.")
                    .arg(QString::fromStdString(svcName)));
        }
    });
    watcher->setFuture(future);
}

void ServiceDashboardMdiWindow::onStopService() {
    if (selectedServiceName_.empty() || !clientManager_ ||
            !clientManager_->isConnected())
        return;

    QPointer<ServiceDashboardMdiWindow> self = this;
    const std::string svcName = selectedServiceName_;

    QFuture<ServiceActionResult> future =
        QtConcurrent::run([self, svcName]() -> ServiceActionResult {
            if (!self || !self->clientManager_) return {};
            controller_api::messaging::stop_service_request req;
            req.service_name = svcName;
            auto resp = self->clientManager_->process_authenticated_request(req);
            ServiceActionResult r;
            r.success = resp && resp->success;
            if (!r.success)
                r.error = QString::fromStdString(
                    resp ? resp->message : "No response from controller");
            return r;
        });

    auto* watcher = new QFutureWatcher<ServiceActionResult>(this);
    connect(watcher, &QFutureWatcher<ServiceActionResult>::finished, this,
            [self, watcher, svcName]() {
        const auto result = watcher->result();
        watcher->deleteLater();
        if (!self) return;
        if (result.success) {
            emit self->statusChanged(
                tr("Stop requested for '%1'.")
                    .arg(QString::fromStdString(svcName)));
            self->refresh();
        } else {
            QMessageBox::critical(self, tr("Stop Service"),
                tr("Failed to stop '%1': %2")
                    .arg(QString::fromStdString(svcName))
                    .arg(result.error));
        }
    });
    watcher->setFuture(future);
}

void ServiceDashboardMdiWindow::onRestartService() {
    if (selectedServiceName_.empty() || !clientManager_ ||
            !clientManager_->isConnected())
        return;

    QPointer<ServiceDashboardMdiWindow> self = this;
    const std::string svcName = selectedServiceName_;

    QFuture<ServiceActionResult> future =
        QtConcurrent::run([self, svcName]() -> ServiceActionResult {
            if (!self || !self->clientManager_) return {};
            controller_api::messaging::restart_service_request req;
            req.service_name = svcName;
            auto resp = self->clientManager_->process_authenticated_request(req);
            ServiceActionResult r;
            r.success = resp && resp->success;
            if (!r.success)
                r.error = QString::fromStdString(
                    resp ? resp->message : "No response from controller");
            return r;
        });

    auto* watcher = new QFutureWatcher<ServiceActionResult>(this);
    connect(watcher, &QFutureWatcher<ServiceActionResult>::finished, this,
            [self, watcher, svcName]() {
        const auto result = watcher->result();
        watcher->deleteLater();
        if (!self) return;
        if (result.success) {
            emit self->statusChanged(
                tr("Restart requested for '%1'.")
                    .arg(QString::fromStdString(svcName)));
            self->refresh();
        } else {
            QMessageBox::critical(self, tr("Restart Service"),
                tr("Failed to restart '%1': %2")
                    .arg(QString::fromStdString(svcName))
                    .arg(result.error));
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
