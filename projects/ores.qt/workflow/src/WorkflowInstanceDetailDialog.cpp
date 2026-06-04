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
#include <QDialogButtonBox>
#include <QHBoxLayout>
#include <QSplitter>
#include <QVBoxLayout>

namespace ores::qt {

WorkflowInstanceDetailDialog::WorkflowInstanceDetailDialog(ClientManager* clientManager,
                                                           const QString& workflowId,
                                                           const QString& workflowType,
                                                           const QString& workflowStatus,
                                                           QWidget* parent)
    : QDialog(parent)
    , clientManager_(clientManager)
    , workflowId_(workflowId)
    , workflowType_(workflowType)
    , workflowStatus_(workflowStatus)
    , statusLabel_(nullptr)
    , stepsWidget_(nullptr)
    , logWidget_(nullptr) {

    setupUi();
    setWindowTitle(tr("Workflow: %1").arg(workflowType_));
}

void WorkflowInstanceDetailDialog::setupUi() {
    setMinimumSize(750, 600);

    auto* layout = new QVBoxLayout(this);

    // ── Header info ───────────────────────────────────────────────────────────
    auto* headerLayout = new QHBoxLayout;

    auto* typeLabel = new QLabel(tr("<b>Type:</b> %1").arg(workflowType_), this);
    headerLayout->addWidget(typeLabel);

    statusLabel_ = new QLabel(tr("<b>Status:</b> %1").arg(workflowStatus_), this);
    headerLayout->addWidget(statusLabel_);

    auto* idLabel = new QLabel(this);
    idLabel->setText(tr("<b>ID:</b> %1").arg(workflowId_));
    idLabel->setTextInteractionFlags(Qt::TextSelectableByMouse);
    headerLayout->addWidget(idLabel);

    headerLayout->addStretch();
    layout->addLayout(headerLayout);

    // ── Steps + log splitter ──────────────────────────────────────────────────
    auto* splitter = new QSplitter(Qt::Vertical, this);

    stepsWidget_ = new WorkflowStepsWidget(clientManager_, splitter);
    splitter->addWidget(stepsWidget_);

    logWidget_ = new WorkflowStepLogWidget(splitter);
    splitter->addWidget(logWidget_);

    splitter->setStretchFactor(0, 2);
    splitter->setStretchFactor(1, 1);

    layout->addWidget(splitter);

    connect(stepsWidget_,
            &WorkflowStepsWidget::stepSelected,
            this,
            [this](const ores::workflow::messaging::workflow_step_summary& step) {
                logWidget_->showLog(QString::fromStdString(step.name), step.log);
            });

    // ── Buttons ───────────────────────────────────────────────────────────────
    auto* buttons = new QDialogButtonBox(QDialogButtonBox::Close, this);
    connect(buttons, &QDialogButtonBox::rejected, this, &QDialog::reject);
    layout->addWidget(buttons);
}

void WorkflowInstanceDetailDialog::loadSteps() {
    stepsWidget_->setInstance(QUuid::fromString(workflowId_));
}

} // namespace ores::qt
