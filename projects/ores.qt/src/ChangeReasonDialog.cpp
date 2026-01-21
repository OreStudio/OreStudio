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
#include "ores.qt/ChangeReasonDialog.hpp"

#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QFormLayout>
#include <QGroupBox>
#include <QStandardItemModel>
#include "ores.dq/domain/change_reason_constants.hpp"

namespace ores::qt {

namespace reason = dq::domain::change_reason_constants;

ChangeReasonDialog::ChangeReasonDialog(
    const std::vector<dq::domain::change_reason>& reasons,
    OperationType operation,
    bool hasFieldChanges,
    QWidget* parent)
    : QDialog(parent),
      reasons_(reasons),
      operation_(operation),
      hasFieldChanges_(hasFieldChanges) {

    setupUi();

    // Initialize with first enabled reason if available
    if (!reasons_.empty()) {
        // Find first enabled item
        for (int i = 0; i < reasonCombo_->count(); ++i) {
            auto* model = qobject_cast<QStandardItemModel*>(reasonCombo_->model());
            if (model) {
                QStandardItem* item = model->item(i);
                if (item && item->isEnabled()) {
                    reasonCombo_->setCurrentIndex(i);
                    onReasonChanged(i);
                    break;
                }
            }
        }
    }
}

void ChangeReasonDialog::setupUi() {
    const QString title = (operation_ == OperationType::Amend)
        ? tr("Change Reason Required")
        : tr("Deletion Reason Required");
    setWindowTitle(title);
    setMinimumWidth(450);

    auto* mainLayout = new QVBoxLayout(this);

    // Header label
    auto* headerLabel = new QLabel(this);
    if (operation_ == OperationType::Amend) {
        headerLabel->setText(tr("Please select a reason for this change:"));
    } else {
        headerLabel->setText(tr("Please select a reason for this deletion:"));
    }
    mainLayout->addWidget(headerLabel);

    // Form layout for reason selection
    auto* formLayout = new QFormLayout();
    formLayout->setFieldGrowthPolicy(QFormLayout::ExpandingFieldsGrow);

    // Reason combo box
    reasonCombo_ = new QComboBox(this);
    reasonCombo_->setMinimumWidth(300);

    // The touch reason code - only valid when no fields have changed
    static const std::string touchReasonCode{reason::codes::non_material_update};

    for (std::size_t i = 0; i < reasons_.size(); ++i) {
        const auto& reason = reasons_[i];
        QString displayText = QString::fromStdString(reason.code);
        reasonCombo_->addItem(displayText, QString::fromStdString(reason.code));

        // Get the model to enable/disable items
        auto* model = qobject_cast<QStandardItemModel*>(reasonCombo_->model());
        if (model) {
            QStandardItem* item = model->item(static_cast<int>(i));
            if (item) {
                bool isTouchReason = (reason.code == touchReasonCode);

                // Touch reason is only valid when no fields changed
                // Other reasons are only valid when fields have changed
                if (hasFieldChanges_) {
                    // Fields changed: disable touch, enable others
                    item->setEnabled(!isTouchReason);
                    if (isTouchReason) {
                        item->setToolTip(tr("Not available when fields have been modified"));
                    }
                } else {
                    // No fields changed (touch operation): only enable touch
                    item->setEnabled(isTouchReason);
                    if (!isTouchReason) {
                        item->setToolTip(tr("Only available when fields have been modified"));
                    }
                }
            }
        }
    }
    formLayout->addRow(tr("Reason:"), reasonCombo_);

    mainLayout->addLayout(formLayout);

    // Description label (shows full description of selected reason)
    descriptionLabel_ = new QLabel(this);
    descriptionLabel_->setWordWrap(true);
    descriptionLabel_->setStyleSheet(
        "QLabel { color: #888888; font-style: italic; padding: 5px; }");
    mainLayout->addWidget(descriptionLabel_);

    // Commentary section
    auto* commentaryGroup = new QGroupBox(tr("Commentary"), this);
    auto* commentaryLayout = new QVBoxLayout(commentaryGroup);

    commentaryLabel_ = new QLabel(this);
    commentaryLabel_->setWordWrap(true);
    commentaryLayout->addWidget(commentaryLabel_);

    commentaryEdit_ = new QTextEdit(this);
    commentaryEdit_->setMaximumHeight(100);
    commentaryEdit_->setPlaceholderText(tr("Enter explanation for this change..."));
    commentaryLayout->addWidget(commentaryEdit_);

    requiredLabel_ = new QLabel(this);
    requiredLabel_->setStyleSheet("QLabel { color: #cc6666; }");
    commentaryLayout->addWidget(requiredLabel_);

    mainLayout->addWidget(commentaryGroup);

    // Spacer
    mainLayout->addStretch();

    // Button box
    buttonBox_ = new QDialogButtonBox(
        QDialogButtonBox::Save | QDialogButtonBox::Cancel, this);
    saveButton_ = buttonBox_->button(QDialogButtonBox::Save);
    saveButton_->setText(operation_ == OperationType::Amend
        ? tr("Save")
        : tr("Confirm Delete"));

    mainLayout->addWidget(buttonBox_);

    // Connections
    connect(reasonCombo_, QOverload<int>::of(&QComboBox::currentIndexChanged),
            this, &ChangeReasonDialog::onReasonChanged);
    connect(commentaryEdit_, &QTextEdit::textChanged,
            this, &ChangeReasonDialog::onCommentaryChanged);
    connect(buttonBox_, &QDialogButtonBox::accepted,
            this, &ChangeReasonDialog::validateAndAccept);
    connect(buttonBox_, &QDialogButtonBox::rejected,
            this, &QDialog::reject);
}

void ChangeReasonDialog::onReasonChanged(int index) {
    if (index < 0 || static_cast<std::size_t>(index) >= reasons_.size()) {
        return;
    }

    const auto& reason = reasons_[static_cast<std::size_t>(index)];

    // Update description
    descriptionLabel_->setText(QString::fromStdString(reason.description));

    // Update commentary requirements
    commentary_required_ = reason.requires_commentary;
    if (commentary_required_) {
        commentaryLabel_->setText(tr("Commentary is required for this reason."));
        requiredLabel_->setText(tr("* Required"));
    } else {
        commentaryLabel_->setText(tr("Commentary is optional for this reason."));
        requiredLabel_->clear();
    }

    updateValidation();
}

void ChangeReasonDialog::onCommentaryChanged() {
    updateValidation();
}

void ChangeReasonDialog::updateValidation() {
    bool valid = true;

    // If commentary is required, it must not be empty
    if (commentary_required_) {
        const QString text = commentaryEdit_->toPlainText().trimmed();
        valid = !text.isEmpty();
    }

    saveButton_->setEnabled(valid);

    // Update required label styling
    if (commentary_required_) {
        const QString text = commentaryEdit_->toPlainText().trimmed();
        if (text.isEmpty()) {
            requiredLabel_->setStyleSheet("QLabel { color: #cc6666; }");
        } else {
            requiredLabel_->setStyleSheet("QLabel { color: #66cc66; }");
        }
    }
}

void ChangeReasonDialog::validateAndAccept() {
    // Final validation check
    if (commentary_required_) {
        const QString text = commentaryEdit_->toPlainText().trimmed();
        if (text.isEmpty()) {
            // Don't accept - validation should have disabled button, but be safe
            return;
        }
    }

    accept();
}

std::string ChangeReasonDialog::selectedReasonCode() const {
    const int index = reasonCombo_->currentIndex();
    if (index < 0 || static_cast<std::size_t>(index) >= reasons_.size()) {
        return {};
    }
    return reasons_[static_cast<std::size_t>(index)].code;
}

std::string ChangeReasonDialog::commentary() const {
    return commentaryEdit_->toPlainText().trimmed().toStdString();
}

}
