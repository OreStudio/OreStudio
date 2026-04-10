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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.qt/WorkflowMdiWindow.hpp"

#include <algorithm>
#include <QFrame>
#include <QPainter>
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QHeaderView>
#include <QApplication>
#include <QInputDialog>
#include <QMessageBox>
#include <QSortFilterProxyModel>
#include <QStyledItemDelegate>
#include <QStyleOptionViewItem>
#include <QtConcurrent>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/DelegatePaintUtils.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/WorkflowInstanceDetailDialog.hpp"
#include "ores.workflow.api/messaging/workflow_query_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;
namespace wf = ores::workflow::messaging;

// ─────────────────────────────────────────────────────────────────────────────
// Local constants
// ─────────────────────────────────────────────────────────────────────────────

namespace {

constexpr std::string_view kWorkflowChangedEvent =
    "ores.workflow.workflow_instance_changed";

// Custom item roles for badge cells
enum ItemRole {
    BadgeTagRole   = Qt::UserRole,
    BadgeColorRole = Qt::UserRole + 1,
    InstanceIdRole = Qt::UserRole + 2,   // stored on col-0 of each instance row
};

// Execution list table columns
enum class Col {
    Status = 0,
    Type,
    Steps,
    CreatedBy,
    CreatedAt,
    CorrelationId,
    Count
};

// Dashboard failures table columns
enum class FCol {
    Type = 0,
    CreatedAt,
    Error,
    Count
};

// ─────────────────────────────────────────────────────────────────────────────
// Badge delegate
// ─────────────────────────────────────────────────────────────────────────────

class BadgeDelegate final : public QStyledItemDelegate {
public:
    explicit BadgeDelegate(QObject* parent = nullptr)
        : QStyledItemDelegate(parent) {}

    void paint(QPainter* painter, const QStyleOptionViewItem& option,
               const QModelIndex& index) const override {
        if (index.data(BadgeTagRole).toString() != QStringLiteral("badge")) {
            QStyledItemDelegate::paint(painter, option, index);
            return;
        }

        QStyleOptionViewItem opt = option;
        initStyleOption(&opt, index);
        QApplication::style()->drawPrimitive(
            QStyle::PE_PanelItemViewItem, &opt, painter);

        const QString text = index.data(Qt::DisplayRole).toString();
        const QColor  bg   = index.data(BadgeColorRole).value<QColor>();
        const QColor  fg   = color_constants::level_text;

        QFont badgeFont = opt.font;
        badgeFont.setPointSize(qRound(badgeFont.pointSize() * 0.8));
        badgeFont.setBold(true);

        DelegatePaintUtils::draw_centered_badge(
            painter, opt.rect, text, bg, fg, badgeFont);
    }

    QSize sizeHint(const QStyleOptionViewItem& option,
                   const QModelIndex& index) const override {
        QSize s = QStyledItemDelegate::sizeHint(option, index);
        if (index.data(BadgeTagRole).toString() == QStringLiteral("badge"))
            s = QSize(qMax(s.width(), 90), qMax(s.height(), 24));
        return s;
    }
};

// ─────────────────────────────────────────────────────────────────────────────
// Helper factories
// ─────────────────────────────────────────────────────────────────────────────

QTableWidgetItem* make_badge_item(const QString& text, const QColor& bg) {
    auto* item = new QTableWidgetItem(text);
    item->setData(BadgeTagRole,   QStringLiteral("badge"));
    item->setData(BadgeColorRole, bg);
    item->setFlags(item->flags() & ~Qt::ItemIsEditable);
    return item;
}

QTableWidgetItem* make_item(const QString& text) {
    auto* item = new QTableWidgetItem(text);
    item->setTextAlignment(Qt::AlignVCenter | Qt::AlignLeft);
    item->setFlags(item->flags() & ~Qt::ItemIsEditable);
    return item;
}

QColor status_color(const QString& status) {
    if (status == QStringLiteral("completed"))
        return color_constants::level_info;
    if (status == QStringLiteral("failed"))
        return color_constants::level_error;
    if (status == QStringLiteral("in_progress") ||
        status == QStringLiteral("compensating"))
        return color_constants::level_warn;
    if (status == QStringLiteral("compensated"))
        return color_constants::level_debug;
    return color_constants::level_trace;
}

} // namespace

// ─────────────────────────────────────────────────────────────────────────────
// WorkflowMdiWindow
// ─────────────────────────────────────────────────────────────────────────────

WorkflowMdiWindow::WorkflowMdiWindow(ClientManager* clientManager,
                                     QWidget* parent)
    : EntityListMdiWindow(parent),
      clientManager_(clientManager),
      toolbar_(nullptr),
      refreshAction_(nullptr),
      autoRefreshAction_(nullptr),
      autoRefreshTimer_(nullptr),
      tabs_(nullptr),
      activeCountLabel_(nullptr),
      compensatingCountLabel_(nullptr),
      failedCountLabel_(nullptr),
      failuresTable_(nullptr),
      searchEdit_(nullptr),
      statusFilter_(nullptr),
      instanceTable_(nullptr),
      watcher_(new QFutureWatcher<FetchResult>(this)) {

    autoRefreshTimer_ = new QTimer(this);
    autoRefreshTimer_->setInterval(60'000); // 1 minute default

    connect(autoRefreshTimer_, &QTimer::timeout,
            this, &WorkflowMdiWindow::reload);
    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &WorkflowMdiWindow::onFetchFinished);

    setupUi();
    setupEventSubscriptions();
    reload();
}

void WorkflowMdiWindow::setupUi() {
    auto* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->setSpacing(0);

    setupToolbar();
    layout->addWidget(toolbar_);
    layout->addWidget(loadingBar());

    setupTabs();
    layout->addWidget(tabs_);
}

void WorkflowMdiWindow::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setIconSize(QSize(16, 16));

    refreshAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::ArrowSync, IconUtils::DefaultIconColor),
        tr("Refresh"));
    refreshAction_->setToolTip(tr("Refresh workflow list"));
    connect(refreshAction_, &QAction::triggered,
            this, &WorkflowMdiWindow::reload);

    toolbar_->addSeparator();

    autoRefreshAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Clock, IconUtils::DefaultIconColor),
        tr("Auto Refresh"));
    autoRefreshAction_->setToolTip(
        tr("Enable automatic refresh; click again to disable"));
    autoRefreshAction_->setCheckable(true);
    autoRefreshAction_->setChecked(false);
    connect(autoRefreshAction_, &QAction::toggled,
            this, &WorkflowMdiWindow::onRefreshToggled);

    initializeStaleIndicator(refreshAction_,
        IconUtils::iconPath(Icon::ArrowSync));
}

void WorkflowMdiWindow::setupTabs() {
    tabs_ = new QTabWidget(this);

    auto* execTab = new QWidget;
    setupExecutionListTab(execTab);
    tabs_->addTab(execTab, tr("Execution List"));

    auto* dashTab = new QWidget;
    setupDashboardTab(dashTab);
    tabs_->addTab(dashTab, tr("Dashboard"));
}

void WorkflowMdiWindow::setupDashboardTab(QWidget* tab) {
    auto* layout = new QVBoxLayout(tab);

    // ── Count cards ──────────────────────────────────────────────────────────
    auto* cardsLayout = new QHBoxLayout;

    auto makeCard = [this](const QString& label, QLabel*& countOut) {
        auto* frame = new QFrame(this);
        frame->setFrameShape(QFrame::StyledPanel);
        auto* fl = new QVBoxLayout(frame);
        auto* title = new QLabel(label, frame);
        title->setAlignment(Qt::AlignCenter);
        countOut = new QLabel(QStringLiteral("0"), frame);
        countOut->setAlignment(Qt::AlignCenter);
        QFont f = countOut->font();
        f.setPointSize(f.pointSize() + 8);
        f.setBold(true);
        countOut->setFont(f);
        fl->addWidget(title);
        fl->addWidget(countOut);
        return frame;
    };

    cardsLayout->addWidget(makeCard(tr("Active"), activeCountLabel_));
    cardsLayout->addWidget(makeCard(tr("Compensating"), compensatingCountLabel_));
    cardsLayout->addWidget(makeCard(tr("Failed"), failedCountLabel_));
    layout->addLayout(cardsLayout);

    // ── Recent failures ──────────────────────────────────────────────────────
    layout->addWidget(new QLabel(tr("Recent Failures:"), tab));

    failuresTable_ = new QTableWidget(0, static_cast<int>(FCol::Count), tab);
    failuresTable_->setHorizontalHeaderLabels(
        {tr("Type"), tr("Created At"), tr("Error")});
    failuresTable_->horizontalHeader()->setStretchLastSection(true);
    failuresTable_->setSelectionBehavior(QAbstractItemView::SelectRows);
    failuresTable_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    failuresTable_->setAlternatingRowColors(true);
    layout->addWidget(failuresTable_);
}

void WorkflowMdiWindow::setupExecutionListTab(QWidget* tab) {
    auto* layout = new QVBoxLayout(tab);

    // ── Filter bar ───────────────────────────────────────────────────────────
    auto* filterLayout = new QHBoxLayout;
    searchEdit_ = new QLineEdit(tab);
    searchEdit_->setPlaceholderText(tr("Search by type or correlation ID…"));
    filterLayout->addWidget(searchEdit_);

    statusFilter_ = new QComboBox(tab);
    statusFilter_->addItem(tr("All Statuses"), QStringLiteral(""));
    statusFilter_->addItem(tr("In Progress"),   QStringLiteral("in_progress"));
    statusFilter_->addItem(tr("Completed"),      QStringLiteral("completed"));
    statusFilter_->addItem(tr("Failed"),         QStringLiteral("failed"));
    statusFilter_->addItem(tr("Compensating"),   QStringLiteral("compensating"));
    statusFilter_->addItem(tr("Compensated"),    QStringLiteral("compensated"));
    filterLayout->addWidget(statusFilter_);
    layout->addLayout(filterLayout);

    // ── Instance table ───────────────────────────────────────────────────────
    instanceTable_ = new QTableWidget(0, static_cast<int>(Col::Count), tab);
    instanceTable_->setHorizontalHeaderLabels(
        {tr("Status"), tr("Type"), tr("Steps"),
         tr("Created By"), tr("Created At"), tr("Correlation ID")});
    instanceTable_->horizontalHeader()->setSectionResizeMode(
        QHeaderView::ResizeToContents);
    instanceTable_->horizontalHeader()->setStretchLastSection(true);
    instanceTable_->setSelectionBehavior(QAbstractItemView::SelectRows);
    instanceTable_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    instanceTable_->setAlternatingRowColors(true);
    instanceTable_->setItemDelegate(new BadgeDelegate(instanceTable_));

    connect(instanceTable_, &QTableWidget::doubleClicked,
            this, &WorkflowMdiWindow::onInstanceDoubleClicked);

    layout->addWidget(instanceTable_);

    connect(searchEdit_, &QLineEdit::textChanged,
            this, &WorkflowMdiWindow::onFilterChanged);
    connect(statusFilter_, QOverload<int>::of(&QComboBox::currentIndexChanged),
            this, &WorkflowMdiWindow::onFilterChanged);
}

void WorkflowMdiWindow::setupEventSubscriptions() {
    if (!clientManager_)
        return;

    connect(clientManager_, &ClientManager::notificationReceived,
            this, &WorkflowMdiWindow::onNotificationReceived);

    auto subscribe_all = [this]() {
        clientManager_->subscribeToEvent(std::string{kWorkflowChangedEvent});
    };

    connect(clientManager_, &ClientManager::loggedIn,
            this, [subscribe_all]() { subscribe_all(); });
    connect(clientManager_, &ClientManager::reconnected,
            this, [subscribe_all]() { subscribe_all(); });

    if (clientManager_->isConnected())
        subscribe_all();
}

// ─────────────────────────────────────────────────────────────────────────────
// doReload / async fetch
// ─────────────────────────────────────────────────────────────────────────────

void WorkflowMdiWindow::doReload() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot reload: not connected.";
        return;
    }

    if (watcher_->isRunning()) {
        BOOST_LOG_SEV(lg(), debug) << "Fetch already in progress; skipping.";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Starting workflow instance fetch.";

    QPointer<WorkflowMdiWindow> self = this;
    watcher_->setFuture(QtConcurrent::run([self]() -> FetchResult {
        if (!self || !self->clientManager_)
            return {};

        try {
            auto resp = self->clientManager_->process_authenticated_request(
                wf::list_workflow_instances_request{});
            if (!resp)
                return {false, QString::fromStdString(resp.error()), {}};
            if (!resp->success)
                return {false, QString::fromStdString(resp->message), {}};
            return {true, {}, std::move(resp->instances)};
        } catch (const std::exception& e) {
            return {false, QString::fromLatin1(e.what()), {}};
        } catch (...) {
            return {false, QStringLiteral("Unknown error fetching workflows"), {}};
        }
    }));
}

void WorkflowMdiWindow::onFetchFinished() {
    const auto result = watcher_->result();
    endLoading();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), warn)
            << "Workflow fetch failed: " << result.error.toStdString();
        emit errorOccurred(result.error);
        return;
    }

    BOOST_LOG_SEV(lg(), debug)
        << "Fetched " << result.instances.size() << " workflow instance(s).";

    currentInstances_ = result.instances;
    populateDashboard(currentInstances_);
    populateExecutionList(currentInstances_);
    emit statusChanged(tr("Loaded %1 workflow(s).")
        .arg(static_cast<int>(currentInstances_.size())));
}

// ─────────────────────────────────────────────────────────────────────────────
// Populate
// ─────────────────────────────────────────────────────────────────────────────

void WorkflowMdiWindow::populateDashboard(
    const std::vector<wf::workflow_instance_summary>& instances) {

    int active = 0, compensating = 0, failed = 0;
    failuresTable_->setRowCount(0);

    for (const auto& inst : instances) {
        const QString status = QString::fromStdString(inst.status);
        if (status == QStringLiteral("in_progress"))   ++active;
        else if (status == QStringLiteral("compensating")) ++compensating;
        else if (status == QStringLiteral("failed")) {
            ++failed;
            const int row = failuresTable_->rowCount();
            failuresTable_->insertRow(row);
            failuresTable_->setItem(row, static_cast<int>(FCol::Type),
                make_item(QString::fromStdString(inst.type)));
            failuresTable_->setItem(row, static_cast<int>(FCol::CreatedAt),
                make_item(QString::fromStdString(inst.created_at)));
            failuresTable_->setItem(row, static_cast<int>(FCol::Error),
                make_item(QString::fromStdString(inst.error)));
        }
    }

    activeCountLabel_->setText(QString::number(active));
    compensatingCountLabel_->setText(QString::number(compensating));
    failedCountLabel_->setText(QString::number(failed));
}

void WorkflowMdiWindow::populateExecutionList(
    const std::vector<wf::workflow_instance_summary>& instances) {

    const QString searchText = searchEdit_->text().trimmed().toLower();
    const QString filterStatus = statusFilter_->currentData().toString();

    instanceTable_->setRowCount(0);

    for (const auto& inst : instances) {
        const QString status = QString::fromStdString(inst.status);
        const QString type   = QString::fromStdString(inst.type);
        const QString corrId = QString::fromStdString(inst.correlation_id);

        // Apply status filter
        if (!filterStatus.isEmpty() && status != filterStatus)
            continue;

        // Apply search filter (type or correlation ID)
        if (!searchText.isEmpty()) {
            if (!type.toLower().contains(searchText) &&
                !corrId.toLower().contains(searchText))
                continue;
        }

        const int row = instanceTable_->rowCount();
        instanceTable_->insertRow(row);

        // Status badge — also store instance ID for detail dialog
        auto* statusItem = make_badge_item(status, status_color(status));
        statusItem->setData(InstanceIdRole, QString::fromStdString(inst.id));
        instanceTable_->setItem(row, static_cast<int>(Col::Status), statusItem);

        instanceTable_->setItem(row, static_cast<int>(Col::Type),
            make_item(type));

        instanceTable_->setItem(row, static_cast<int>(Col::Steps),
            make_item(QStringLiteral("%1/%2")
                .arg(inst.current_step_index)
                .arg(inst.step_count)));

        instanceTable_->setItem(row, static_cast<int>(Col::CreatedBy),
            make_item(QString::fromStdString(inst.created_by)));

        instanceTable_->setItem(row, static_cast<int>(Col::CreatedAt),
            make_item(QString::fromStdString(inst.created_at)));

        instanceTable_->setItem(row, static_cast<int>(Col::CorrelationId),
            make_item(corrId));
    }

    instanceTable_->resizeColumnsToContents();
    instanceTable_->horizontalHeader()->setStretchLastSection(true);
}

// ─────────────────────────────────────────────────────────────────────────────
// Slots
// ─────────────────────────────────────────────────────────────────────────────

void WorkflowMdiWindow::onRefreshToggled(bool checked) {
    if (checked) {
        const int current_secs = autoRefreshTimer_->interval() / 1000;
        const int secs = QInputDialog::getInt(
            this, tr("Auto Refresh"),
            tr("Refresh interval (seconds):"),
            current_secs, 5, 3600, 5);
        if (secs <= 0) {
            QSignalBlocker blocker(autoRefreshAction_);
            autoRefreshAction_->setChecked(false);
            return;
        }
        autoRefreshTimer_->setInterval(secs * 1000);
        autoRefreshAction_->setToolTip(
            tr("Auto refresh every %1 s — click to disable").arg(secs));
        autoRefreshTimer_->start();
    } else {
        autoRefreshTimer_->stop();
        autoRefreshAction_->setToolTip(
            tr("Enable automatic refresh; click again to disable"));
    }
}

void WorkflowMdiWindow::onInstanceDoubleClicked(const QModelIndex& index) {
    const int row = index.row();
    if (row < 0 || row >= instanceTable_->rowCount())
        return;

    auto* statusItem = instanceTable_->item(row, static_cast<int>(Col::Status));
    auto* typeItem   = instanceTable_->item(row, static_cast<int>(Col::Type));
    if (!statusItem || !typeItem)
        return;

    const QString instanceId = statusItem->data(InstanceIdRole).toString();
    const QString type       = typeItem->text();
    const QString status     = statusItem->text();

    if (instanceId.isEmpty())
        return;

    auto* dlg = new WorkflowInstanceDetailDialog(
        clientManager_, instanceId, type, status, this);
    dlg->setAttribute(Qt::WA_DeleteOnClose);
    dlg->loadSteps();
    dlg->exec();
}

void WorkflowMdiWindow::onNotificationReceived(
    const QString& eventType,
    const QDateTime& /*timestamp*/,
    const QStringList& /*entityIds*/,
    const QString& /*tenantId*/) {

    if (eventType == QLatin1String(kWorkflowChangedEvent))
        markAsStale();
}

void WorkflowMdiWindow::onFilterChanged() {
    // Re-apply filters to the already-loaded instances without a server round-trip.
    populateExecutionList(currentInstances_);
}

} // namespace ores::qt
