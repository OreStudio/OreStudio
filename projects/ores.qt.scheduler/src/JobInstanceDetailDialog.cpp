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
#include "ores.qt/JobInstanceDetailDialog.hpp"

#include <QVBoxLayout>
#include <QFormLayout>
#include <QLabel>
#include <QLineEdit>
#include <QPlainTextEdit>
#include <QDialogButtonBox>
#include <QGroupBox>

namespace ores::qt {

namespace {

QLineEdit* readOnlyField(const QString& text, QWidget* parent) {
    auto* e = new QLineEdit(text, parent);
    e->setReadOnly(true);
    return e;
}

} // namespace

JobInstanceDetailDialog::JobInstanceDetailDialog(
    const scheduler::messaging::job_instance_summary& inst,
    QWidget* parent)
    : QDialog(parent) {

    setWindowTitle(tr("Job Instance — %1").arg(
        QString::fromStdString(inst.job_name)));
    setAttribute(Qt::WA_DeleteOnClose);
    resize(560, 400);

    auto* mainLayout = new QVBoxLayout(this);

    // Execution details group
    auto* grp = new QGroupBox(tr("Execution Details"), this);
    auto* form = new QFormLayout(grp);

    form->addRow(tr("Job Name:"),
        readOnlyField(QString::fromStdString(inst.job_name), grp));
    form->addRow(tr("Status:"),
        readOnlyField(QString::fromStdString(inst.status), grp));
    form->addRow(tr("Action Type:"),
        readOnlyField(QString::fromStdString(inst.action_type), grp));
    form->addRow(tr("Triggered At:"),
        readOnlyField(QString::fromStdString(inst.triggered_at), grp));
    form->addRow(tr("Started At:"),
        readOnlyField(QString::fromStdString(inst.started_at), grp));
    form->addRow(tr("Completed At:"),
        readOnlyField(inst.completed_at
            ? QString::fromStdString(*inst.completed_at) : tr("—"), grp));
    form->addRow(tr("Duration:"),
        readOnlyField(inst.duration_ms
            ? tr("%1 ms").arg(*inst.duration_ms) : tr("—"), grp));
    form->addRow(tr("Job Definition ID:"),
        readOnlyField(QString::fromStdString(inst.job_definition_id), grp));

    mainLayout->addWidget(grp);

    if (!inst.error_message.empty()) {
        auto* errGrp = new QGroupBox(tr("Error"), this);
        auto* errLayout = new QVBoxLayout(errGrp);
        auto* errEdit = new QPlainTextEdit(
            QString::fromStdString(inst.error_message), errGrp);
        errEdit->setReadOnly(true);
        errEdit->setMaximumHeight(100);
        errLayout->addWidget(errEdit);
        mainLayout->addWidget(errGrp);
    }

    auto* buttons = new QDialogButtonBox(QDialogButtonBox::Close, this);
    connect(buttons, &QDialogButtonBox::rejected, this, &QDialog::reject);
    mainLayout->addWidget(buttons);
}

}
