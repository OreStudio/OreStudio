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
#include "ores.qt/WorkflowInstanceDetailDialog.hpp"

#include <QPainter>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QApplication>
#include <QDialogButtonBox>
#include <QStyledItemDelegate>
#include <QStyleOptionViewItem>
#include <QtConcurrent>
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/DelegatePaintUtils.hpp"
#include "ores.workflow.api/messaging/workflow_query_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;
namespace wf = ores::workflow::messaging;

namespace {

// Custom item roles for badge cells
enum ItemRole {
    BadgeTagRole   = Qt::UserRole,
    BadgeColorRole = Qt::UserRole + 1,
};

// Step table columns
enum class Col {
    Index = 0,
    Name,
    Status,
    StartedAt,
    CompletedAt,
    Error,
    Count
};

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
// WorkflowInstanceDetailDialog
// ─────────────────────────────────────────────────────────────────────────────

WorkflowInstanceDetailDialog::WorkflowInstanceDetailDialog(
    ClientManager* clientManager,
    const QString& workflowId,
    const QString& workflowType,
    const QString& workflowStatus,
    QWidget* parent)
    : QDialog(parent),
      clientManager_(clientManager),
      workflowId_(workflowId),
      workflowType_(workflowType),
      workflowStatus_(workflowStatus),
      statusLabel_(nullptr),
      stepsTable_(nullptr),
      watcher_(new QFutureWatcher<FetchResult>(this)) {

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &WorkflowInstanceDetailDialog::onFetchFinished);

    setupUi();
    setWindowTitle(tr("Workflow: %1").arg(workflowType_));
}

void WorkflowInstanceDetailDialog::setupUi() {
    setMinimumSize(700, 450);

    auto* layout = new QVBoxLayout(this);

    // ── Header info ───────────────────────────────────────────────────────────
    auto* headerLayout = new QHBoxLayout;

    auto* typeLabel = new QLabel(
        tr("<b>Type:</b> %1").arg(workflowType_), this);
    headerLayout->addWidget(typeLabel);

    statusLabel_ = new QLabel(
        tr("<b>Status:</b> %1").arg(workflowStatus_), this);
    headerLayout->addWidget(statusLabel_);

    auto* idLabel = new QLabel(this);
    idLabel->setText(tr("<b>ID:</b> %1").arg(workflowId_));
    idLabel->setTextInteractionFlags(Qt::TextSelectableByMouse);
    headerLayout->addWidget(idLabel);

    headerLayout->addStretch();
    layout->addLayout(headerLayout);

    // ── Steps table ───────────────────────────────────────────────────────────
    stepsTable_ = new QTableWidget(0, static_cast<int>(Col::Count), this);
    stepsTable_->setHorizontalHeaderLabels(
        {tr("#"), tr("Name"), tr("Status"),
         tr("Started At"), tr("Completed At"), tr("Error")});
    stepsTable_->horizontalHeader()->setSectionResizeMode(
        QHeaderView::ResizeToContents);
    stepsTable_->horizontalHeader()->setStretchLastSection(true);
    stepsTable_->setSelectionBehavior(QAbstractItemView::SelectRows);
    stepsTable_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    stepsTable_->setAlternatingRowColors(true);
    stepsTable_->setItemDelegate(new BadgeDelegate(stepsTable_));
    layout->addWidget(stepsTable_);

    // ── Buttons ───────────────────────────────────────────────────────────────
    auto* buttons = new QDialogButtonBox(QDialogButtonBox::Close, this);
    connect(buttons, &QDialogButtonBox::rejected, this, &QDialog::reject);
    layout->addWidget(buttons);
}

void WorkflowInstanceDetailDialog::loadSteps() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        statusLabel_->setText(tr("<b>Status:</b> %1 (not connected)")
            .arg(workflowStatus_));
        return;
    }

    BOOST_LOG_SEV(lg(), debug)
        << "Loading steps for workflow " << workflowId_.toStdString();

    QPointer<WorkflowInstanceDetailDialog> self = this;
    const std::string id = workflowId_.toStdString();

    watcher_->setFuture(QtConcurrent::run([self, id]() -> FetchResult {
        if (!self || !self->clientManager_)
            return {};

        try {
            wf::get_workflow_steps_request req;
            req.workflow_instance_id = id;
            auto resp = self->clientManager_->process_authenticated_request(req);
            if (!resp)
                return {false, QString::fromStdString(resp.error()), {}};
            if (!resp->success)
                return {false, QString::fromStdString(resp->message), {}};
            return {true, {}, std::move(resp->steps)};
        } catch (const std::exception& e) {
            return {false, QString::fromLatin1(e.what()), {}};
        } catch (...) {
            return {false, QStringLiteral("Unknown error loading steps"), {}};
        }
    }));
}

void WorkflowInstanceDetailDialog::onFetchFinished() {
    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), warn)
            << "Step fetch failed: " << result.error.toStdString();
        statusLabel_->setText(tr("<b>Status:</b> %1 — Error: %2")
            .arg(workflowStatus_, result.error));
        return;
    }

    populateSteps(result.steps);
}

void WorkflowInstanceDetailDialog::populateSteps(
    const std::vector<wf::workflow_step_summary>& steps) {

    stepsTable_->setRowCount(0);

    for (const auto& step : steps) {
        const int row = stepsTable_->rowCount();
        stepsTable_->insertRow(row);

        stepsTable_->setItem(row, static_cast<int>(Col::Index),
            make_item(QString::number(step.step_index)));

        stepsTable_->setItem(row, static_cast<int>(Col::Name),
            make_item(QString::fromStdString(step.name)));

        const QString status = QString::fromStdString(step.status);
        stepsTable_->setItem(row, static_cast<int>(Col::Status),
            make_badge_item(status, status_color(status)));

        stepsTable_->setItem(row, static_cast<int>(Col::StartedAt),
            make_item(step.started_at
                ? QString::fromStdString(*step.started_at)
                : QStringLiteral("—")));

        stepsTable_->setItem(row, static_cast<int>(Col::CompletedAt),
            make_item(step.completed_at
                ? QString::fromStdString(*step.completed_at)
                : QStringLiteral("—")));

        stepsTable_->setItem(row, static_cast<int>(Col::Error),
            make_item(QString::fromStdString(step.error)));
    }

    stepsTable_->resizeColumnsToContents();
    stepsTable_->horizontalHeader()->setStretchLastSection(true);
}

} // namespace ores::qt
