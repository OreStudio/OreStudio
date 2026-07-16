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
#include "ores.qt/CurrencyPairConventionDetailDialog.hpp"
#include "ores.qt/BadgeComboHelper.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.refdata.api/messaging/protocol.hpp"
#include "ui_CurrencyPairConventionDetailDialog.h"
#include <QComboBox>
#include <QFutureWatcher>
#include <QMessageBox>
#include <QtConcurrent>
#include <algorithm>

namespace ores::qt {

using namespace ores::logging;

CurrencyPairConventionDetailDialog::CurrencyPairConventionDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::CurrencyPairConventionDetailDialog)
    , clientManager_(nullptr) {

    ui_->setupUi(this);
    WidgetUtils::setupComboBoxes(this);
    setupUi();
    setupCombos();
    setupConnections();
    // Hierarchy tree seam: a future :implements 9B165431-2921-4CAC-A2E8-2C186741E523
    // block is expected to construct a HierarchyModelBuilder-derived model
    // for this entity, wrap it in a HierarchyTreeWidget, and insert that
    // widget into this dialog's layout (e.g. a dedicated tab). Left empty
    // when no entity implements this kind.
    // Composite child-entity tables seam: an :implements
    // 7E4A2C8D-9F1B-4E6A-8D3C-5B2A7E9F1C4D block constructs one QTableWidget
    // + QToolBar per embedded child entity (e.g. identifiers, contact
    // information), wraps each in a tab, and inserts it into this dialog's
    // tab widget. Left empty when no entity implements this kind.
}

CurrencyPairConventionDetailDialog::~CurrencyPairConventionDetailDialog() {
    delete ui_;
}

QTabWidget* CurrencyPairConventionDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* CurrencyPairConventionDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* CurrencyPairConventionDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString CurrencyPairConventionDetailDialog::code() const {
    return QString::fromStdString(convention_.pair_code);
}

void CurrencyPairConventionDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void CurrencyPairConventionDetailDialog::setupCombos() {
    ui_->businessDayConventionCombo->clear();
    ui_->businessDayConventionCombo->addItem(tr("Following"), QString("Following"));
    ui_->businessDayConventionCombo->addItem(tr("ModifiedFollowing"), QString("ModifiedFollowing"));
    ui_->businessDayConventionCombo->addItem(tr("Preceding"), QString("Preceding"));
    ui_->businessDayConventionCombo->addItem(tr("ModifiedPreceding"), QString("ModifiedPreceding"));
    ui_->businessDayConventionCombo->addItem(tr("Unadjusted"), QString("Unadjusted"));
    ui_->businessDayConventionCombo->addItem(tr("HalfMonthModifiedFollowing"),
                                             QString("HalfMonthModifiedFollowing"));
    ui_->businessDayConventionCombo->addItem(tr("Nearest"), QString("Nearest"));
}

void CurrencyPairConventionDetailDialog::setupConnections() {
    connect(ui_->saveButton,
            &QPushButton::clicked,
            this,
            &CurrencyPairConventionDetailDialog::onSaveClicked);
    connect(ui_->deleteButton,
            &QPushButton::clicked,
            this,
            &CurrencyPairConventionDetailDialog::onDeleteClicked);
    connect(ui_->closeButton,
            &QPushButton::clicked,
            this,
            &CurrencyPairConventionDetailDialog::onCloseClicked);

    connect(ui_->pairCodeCombo,
            &QComboBox::currentIndexChanged,
            this,
            &CurrencyPairConventionDetailDialog::onFieldChanged);
    connect(ui_->pipFactorEdit,
            &QLineEdit::textChanged,
            this,
            &CurrencyPairConventionDetailDialog::onFieldChanged);
    connect(ui_->tickSizeEdit,
            &QLineEdit::textChanged,
            this,
            &CurrencyPairConventionDetailDialog::onFieldChanged);
    connect(ui_->advanceCalendarEdit,
            &QLineEdit::textChanged,
            this,
            &CurrencyPairConventionDetailDialog::onFieldChanged);
    connect(ui_->businessDayConventionCombo,
            &QComboBox::currentIndexChanged,
            this,
            &CurrencyPairConventionDetailDialog::onFieldChanged);
    connect(ui_->spotRelativeCheckBox,
            &QCheckBox::toggled,
            this,
            &CurrencyPairConventionDetailDialog::onFieldChanged);
    connect(ui_->endOfMonthCheckBox,
            &QCheckBox::toggled,
            this,
            &CurrencyPairConventionDetailDialog::onFieldChanged);
}

void CurrencyPairConventionDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    setup_badge_combo(this,
                      ui_->businessDayConventionCombo,
                      badgeCache(),
                      "currency_pair_convention_business_day_convention");
    populatePairCodeCombo();
}

void CurrencyPairConventionDetailDialog::populatePairCodeCombo() {
    if (!clientManager_ || !clientManager_->isConnected())
        return;

    QPointer<CurrencyPairConventionDetailDialog> self = this;
    auto* watcher = new QFutureWatcher<std::vector<std::string>>(self);
    QObject::connect(
        watcher, &QFutureWatcher<std::vector<std::string>>::finished, self, [self, watcher]() {
            auto codes = watcher->result();
            watcher->deleteLater();
            if (!self)
                return;

            auto* combo = self->ui_->pairCodeCombo;
            const QString previous = combo->currentText();
            combo->blockSignals(true);
            combo->clear();
            for (const auto& c : codes)
                combo->addItem(QString::fromStdString(c));
            // fallback_selection is evaluated here (fetch-completion time), not
            // at populate-call time, since setConvention() may run before or
            // after setClientManager() triggers this fetch.
            const QString fallback = QString::fromStdString(self->convention_.pair_code);
            const QString to_select = !previous.isEmpty() ? previous : fallback;
            if (!to_select.isEmpty()) {
                const int idx = combo->findText(to_select);
                if (idx >= 0)
                    combo->setCurrentIndex(idx);
            }
            combo->blockSignals(false);

            if (self->imageCache())
                apply_flag_icons(
                    combo, self->imageCache(), FlagSource::CurrencyPair, currency_pair_icon_size());
        });

    auto* cm = clientManager_;
    watcher->setFuture(QtConcurrent::run([cm]() { return fetch_currency_pair_codes(cm); }));
}

void CurrencyPairConventionDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void CurrencyPairConventionDetailDialog::setConvention(
    const refdata::domain::currency_pair_convention& convention) {
    convention_ = convention;
    updateUiFromConvention();
}

void CurrencyPairConventionDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->pairCodeCombo->setEnabled(createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
    WidgetUtils::set_combo_locked(ui_->pairCodeCombo, !createMode);
}

void CurrencyPairConventionDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CurrencyPairConventionDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->pairCodeCombo->setEnabled(false);
    ui_->pipFactorEdit->setReadOnly(readOnly);
    ui_->tickSizeEdit->setReadOnly(readOnly);
    ui_->advanceCalendarEdit->setReadOnly(readOnly);
    ui_->businessDayConventionCombo->setEnabled(!readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
    WidgetUtils::set_combo_locked(ui_->pairCodeCombo, true);
}

void CurrencyPairConventionDetailDialog::updateUiFromConvention() {
    {
        const auto val = QString::fromStdString(convention_.pair_code);
        const int idx = ui_->pairCodeCombo->findText(val);
        ui_->pairCodeCombo->setCurrentIndex(idx);
    }
    ui_->pipFactorEdit->setText(QString::number(convention_.pip_factor));
    ui_->tickSizeEdit->setText(QString::number(convention_.tick_size));
    ui_->decimalPlacesSpinBox->setValue(convention_.decimal_places);
    ui_->advanceCalendarEdit->setText(convention_.advance_calendar ?
                                          QString::fromStdString(*convention_.advance_calendar) :
                                          QString{});
    {
        const auto val = convention_.business_day_convention ?
                             QString::fromStdString(*convention_.business_day_convention) :
                             QString{};
        const int idx = ui_->businessDayConventionCombo->findData(val);
        if (idx >= 0)
            ui_->businessDayConventionCombo->setCurrentIndex(idx);
    }
    ui_->spotRelativeCheckBox->setCheckState(
        convention_.spot_relative ? (*convention_.spot_relative ? Qt::Checked : Qt::Unchecked) :
                                    Qt::PartiallyChecked);
    ui_->endOfMonthCheckBox->setCheckState(
        convention_.end_of_month ? (*convention_.end_of_month ? Qt::Checked : Qt::Unchecked) :
                                   Qt::PartiallyChecked);

    populateProvenance(convention_.version,
                       convention_.modified_by,
                       convention_.performed_by,
                       convention_.recorded_at,
                       convention_.change_reason_code,
                       convention_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void CurrencyPairConventionDetailDialog::updateConventionFromUi() {
    if (createMode_) {
        convention_.pair_code = ui_->pairCodeCombo->currentText().trimmed().toStdString();
    }
    convention_.pip_factor = ui_->pipFactorEdit->text().trimmed().toDouble();
    convention_.tick_size = ui_->tickSizeEdit->text().trimmed().toDouble();
    convention_.decimal_places = ui_->decimalPlacesSpinBox->value();
    {
        const auto advance_calendar_str = ui_->advanceCalendarEdit->text().trimmed().toStdString();
        convention_.advance_calendar = advance_calendar_str.empty() ?
                                           std::nullopt :
                                           std::optional<std::string>(advance_calendar_str);
    }
    convention_.business_day_convention =
        ui_->businessDayConventionCombo->currentData().toString().toStdString();
    switch (ui_->spotRelativeCheckBox->checkState()) {
        case Qt::Checked:
            convention_.spot_relative = std::optional<bool>(true);
            break;
        case Qt::Unchecked:
            convention_.spot_relative = std::optional<bool>(false);
            break;
        default:
            convention_.spot_relative = std::nullopt;
            break;
    }
    switch (ui_->endOfMonthCheckBox->checkState()) {
        case Qt::Checked:
            convention_.end_of_month = std::optional<bool>(true);
            break;
        case Qt::Unchecked:
            convention_.end_of_month = std::optional<bool>(false);
            break;
        default:
            convention_.end_of_month = std::nullopt;
            break;
    }
    convention_.modified_by = username_;
}


void CurrencyPairConventionDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CurrencyPairConventionDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool CurrencyPairConventionDetailDialog::validateInput() {
    const QString pip_factor_val = ui_->pipFactorEdit->text().trimmed();
    const QString tick_size_val = ui_->tickSizeEdit->text().trimmed();
    const bool pair_code_selected = ui_->pairCodeCombo->currentIndex() >= 0;

    return true && !pip_factor_val.isEmpty() && !tick_size_val.isEmpty() && pair_code_selected;
}

void CurrencyPairConventionDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot save currency pair convention while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input", "Please fill in all required fields.");
        return;
    }

    if (createMode_) {
        const std::string selectedPairCode = ui_->pairCodeCombo->currentText().toStdString();

        refdata::messaging::get_currency_pair_conventions_request checkRequest;
        checkRequest.limit = lookup_fetch_limit;
        auto checkResult = clientManager_->process_authenticated_request(std::move(checkRequest));
        if (!checkResult) {
            MessageBoxHelper::warning(
                this,
                "Cannot Verify",
                "Could not check for an existing convention before saving. Please try again.");
            return;
        }

        const bool exists = std::ranges::any_of(checkResult->conventions, [&](const auto& c) {
            return c.pair_code == selectedPairCode;
        });
        if (exists) {
            MessageBoxHelper::warning(
                this,
                "Duplicate Convention",
                QString("'%1' already has a convention. Edit the existing one instead.")
                    .arg(QString::fromStdString(selectedPairCode)));
            return;
        }
    }

    const auto crOpType = createMode_ ? ChangeReasonDialog::OperationType::Create :
                                        ChangeReasonDialog::OperationType::Amend;
    const auto crSel = promptChangeReason(crOpType, hasChanges_, createMode_ ? "system" : "common");
    if (!crSel)
        return;
    convention_.change_reason_code = crSel->reason_code;
    convention_.change_commentary = crSel->commentary;

    updateConventionFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving currency pair convention: " << convention_.pair_code;

    QPointer<CurrencyPairConventionDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, convention = convention_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_currency_pair_convention_request request;
        request.data = convention;
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished, self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Currency Pair Convention saved successfully";
            QString code = QString::fromStdString(self->convention_.pair_code);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->conventionSaved(code);
            self->notifySaveSuccess(tr("Currency Pair Convention '%1' saved").arg(code));
        } else {
            BOOST_LOG_SEV(lg(), error) << "Save failed: " << result.message;
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Save Failed", errorMsg);
        }
    });

    QFuture<SaveResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void CurrencyPairConventionDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot delete currency pair convention while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(convention_.pair_code);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Currency Pair Convention",
        QString("Are you sure you want to delete currency pair convention '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting currency pair convention: " << convention_.pair_code;

    QPointer<CurrencyPairConventionDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, code = convention_.pair_code]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_currency_pair_convention_request request;
        request.pair_codes = {code};
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished, self, [self, code, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Currency Pair Convention deleted successfully";
            emit self->statusMessage(QString("Currency Pair Convention '%1' deleted").arg(code));
            emit self->conventionDeleted(code);
            self->requestClose();
        } else {
            BOOST_LOG_SEV(lg(), error) << "Delete failed: " << result.message;
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Delete Failed", errorMsg);
        }
    });

    QFuture<DeleteResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}


}
