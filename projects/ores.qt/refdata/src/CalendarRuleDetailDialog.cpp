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
#include "ores.qt/CalendarRuleDetailDialog.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/DynamicComboSetup.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.refdata.api/messaging/calendar_rule_protocol.hpp"
#include "ui_CalendarRuleDetailDialog.h"
#include <QComboBox>
#include <QFutureWatcher>
#include <QMessageBox>
#include <QtConcurrent>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

CalendarRuleDetailDialog::CalendarRuleDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::CalendarRuleDetailDialog)
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

CalendarRuleDetailDialog::~CalendarRuleDetailDialog() {
    delete ui_;
}

QTabWidget* CalendarRuleDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* CalendarRuleDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* CalendarRuleDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString CalendarRuleDetailDialog::code() const {
    return QString::fromStdString(boost::uuids::to_string(rule_.id));
}

void CalendarRuleDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void CalendarRuleDetailDialog::setupCombos() {}

void CalendarRuleDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this, &CalendarRuleDetailDialog::onSaveClicked);
    connect(
        ui_->deleteButton, &QPushButton::clicked, this, &CalendarRuleDetailDialog::onDeleteClicked);
    connect(
        ui_->closeButton, &QPushButton::clicked, this, &CalendarRuleDetailDialog::onCloseClicked);

    connect(ui_->idEdit, &QLineEdit::textChanged, this, &CalendarRuleDetailDialog::onCodeChanged);
    connect(ui_->calendarCombo,
            &QComboBox::currentIndexChanged,
            this,
            &CalendarRuleDetailDialog::onFieldChanged);
    connect(
        ui_->kindEdit, &QLineEdit::textChanged, this, &CalendarRuleDetailDialog::onFieldChanged);
    connect(
        ui_->monthSpin, &QSpinBox::valueChanged, this, &CalendarRuleDetailDialog::onFieldChanged);
    connect(ui_->daySpin, &QSpinBox::valueChanged, this, &CalendarRuleDetailDialog::onFieldChanged);
    connect(
        ui_->weekdaySpin, &QSpinBox::valueChanged, this, &CalendarRuleDetailDialog::onFieldChanged);
    connect(ui_->occurrenceSpin,
            &QSpinBox::valueChanged,
            this,
            &CalendarRuleDetailDialog::onFieldChanged);
    connect(ui_->dayOffsetSpin,
            &QSpinBox::valueChanged,
            this,
            &CalendarRuleDetailDialog::onFieldChanged);
    connect(
        ui_->shiftEdit, &QLineEdit::textChanged, this, &CalendarRuleDetailDialog::onFieldChanged);
    connect(ui_->effectiveFromSpin,
            &QSpinBox::valueChanged,
            this,
            &CalendarRuleDetailDialog::onFieldChanged);
    connect(ui_->effectiveToSpin,
            &QSpinBox::valueChanged,
            this,
            &CalendarRuleDetailDialog::onFieldChanged);
}

void CalendarRuleDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    populateCalendarCode();
}

void CalendarRuleDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void CalendarRuleDetailDialog::setRule(const refdata::domain::calendar_rule& rule) {
    rule_ = rule;
    updateUiFromRule();
}

void CalendarRuleDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->idEdit->setReadOnly(true);
    ui_->calendarCombo->setEnabled(createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        rule_.id = boost::uuids::random_generator()();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void CalendarRuleDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CalendarRuleDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->idEdit->setReadOnly(true);
    ui_->calendarCombo->setEnabled(false);
    ui_->kindEdit->setReadOnly(readOnly);
    ui_->shiftEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void CalendarRuleDetailDialog::populateCalendarCode() {
    BOOST_LOG_SEV(lg(), debug) << "Populating calendar_code combo";
    populateDynamicCombo<refdata::domain::calendar>(
        ui_->calendarCombo,
        this,
        clientManager_,
        &fetch_calendars,
        "calendarRuleCalendarWatcher",
        [](const auto& t) { return QString::fromStdString(t.name); },
        [](const auto& t) { return QString::fromStdString(t.code); },
        [](const auto& t) { return t.version; },
        [this]() { return QString::fromStdString(rule_.calendar_code); },
        [this](const QString& error) {
            emit errorMessage(tr("Failed to load calendars: %1").arg(error));
        },
        []() {},
        QObject::tr("Loading…"),
        QObject::tr("Failed to load"),
        [](const auto& t) { return QString::fromStdString(t.code); },
        [](const auto&) { return false; },
        QString{});
}
void CalendarRuleDetailDialog::updateUiFromRule() {
    ui_->idEdit->setText(QString::fromStdString(boost::uuids::to_string(rule_.id)));
    {
        const auto val = QString::fromStdString(rule_.calendar_code);
        const int idx = ui_->calendarCombo->findData(val);
        if (idx >= 0)
            ui_->calendarCombo->setCurrentIndex(idx);
    }
    ui_->kindEdit->setText(QString::fromStdString(rule_.kind));
    ui_->monthSpin->setValue(rule_.month.value_or(ui_->monthSpin->minimum()));
    ui_->daySpin->setValue(rule_.day.value_or(ui_->daySpin->minimum()));
    ui_->weekdaySpin->setValue(rule_.weekday.value_or(ui_->weekdaySpin->minimum()));
    ui_->occurrenceSpin->setValue(rule_.occurrence.value_or(ui_->occurrenceSpin->minimum()));
    ui_->dayOffsetSpin->setValue(rule_.day_offset.value_or(ui_->dayOffsetSpin->minimum()));
    ui_->shiftEdit->setText(QString::fromStdString(rule_.shift));
    ui_->effectiveFromSpin->setValue(
        rule_.effective_from.value_or(ui_->effectiveFromSpin->minimum()));
    ui_->effectiveToSpin->setValue(rule_.effective_to.value_or(ui_->effectiveToSpin->minimum()));

    populateProvenance(rule_.version,
                       rule_.modified_by,
                       rule_.performed_by,
                       rule_.recorded_at,
                       rule_.change_reason_code,
                       rule_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void CalendarRuleDetailDialog::updateRuleFromUi() {
    if (createMode_) {
        rule_.calendar_code = ui_->calendarCombo->currentData().toString().trimmed().toStdString();
    }
    rule_.kind = ui_->kindEdit->text().trimmed().toStdString();
    if (ui_->monthSpin->value() == ui_->monthSpin->minimum())
        rule_.month = std::nullopt;
    else
        rule_.month = ui_->monthSpin->value();
    if (ui_->daySpin->value() == ui_->daySpin->minimum())
        rule_.day = std::nullopt;
    else
        rule_.day = ui_->daySpin->value();
    if (ui_->weekdaySpin->value() == ui_->weekdaySpin->minimum())
        rule_.weekday = std::nullopt;
    else
        rule_.weekday = ui_->weekdaySpin->value();
    if (ui_->occurrenceSpin->value() == ui_->occurrenceSpin->minimum())
        rule_.occurrence = std::nullopt;
    else
        rule_.occurrence = ui_->occurrenceSpin->value();
    if (ui_->dayOffsetSpin->value() == ui_->dayOffsetSpin->minimum())
        rule_.day_offset = std::nullopt;
    else
        rule_.day_offset = ui_->dayOffsetSpin->value();
    rule_.shift = ui_->shiftEdit->text().trimmed().toStdString();
    if (ui_->effectiveFromSpin->value() == ui_->effectiveFromSpin->minimum())
        rule_.effective_from = std::nullopt;
    else
        rule_.effective_from = ui_->effectiveFromSpin->value();
    if (ui_->effectiveToSpin->value() == ui_->effectiveToSpin->minimum())
        rule_.effective_to = std::nullopt;
    else
        rule_.effective_to = ui_->effectiveToSpin->value();
    rule_.modified_by = username_;
}

void CalendarRuleDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CalendarRuleDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CalendarRuleDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool CalendarRuleDetailDialog::validateInput() {
    const QString id_val = ui_->idEdit->text().trimmed();
    const QString kind_val = ui_->kindEdit->text().trimmed();
    const QString shift_val = ui_->shiftEdit->text().trimmed();
    const bool calendar_code_selected = ui_->calendarCombo->currentIndex() >= 0;

    return true && !id_val.isEmpty() && !kind_val.isEmpty() && !shift_val.isEmpty() &&
           calendar_code_selected;
}

void CalendarRuleDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot save calendar rule while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input", "Please fill in all required fields.");
        return;
    }


    const auto crOpType = createMode_ ? ChangeReasonDialog::OperationType::Create :
                                        ChangeReasonDialog::OperationType::Amend;
    const auto crSel = promptChangeReason(crOpType, hasChanges_, createMode_ ? "system" : "common");
    if (!crSel)
        return;
    rule_.change_reason_code = crSel->reason_code;
    rule_.change_commentary = crSel->commentary;

    updateRuleFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving calendar rule: " << boost::uuids::to_string(rule_.id);

    QPointer<CalendarRuleDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, rule = rule_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_calendar_rule_request request;
        request.data = rule;
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher,
            &QFutureWatcher<SaveResult>::finished,
            self,
            [self, watcher, crReasonCode = crSel->reason_code, crCommentary = crSel->commentary]() {
                auto result = watcher->result();
                watcher->deleteLater();

                if (result.success) {
                    BOOST_LOG_SEV(lg(), info) << "Calendar Rule saved successfully";
                    QString code = QString::fromStdString(boost::uuids::to_string(self->rule_.id));
                    self->hasChanges_ = false;
                    self->updateSaveButtonState();
                    emit self->ruleSaved(code);
                    self->notifySaveSuccess(tr("Calendar Rule '%1' saved").arg(code));
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

void CalendarRuleDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot delete calendar rule while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(boost::uuids::to_string(rule_.id));
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Calendar Rule",
        QString("Are you sure you want to delete calendar rule '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting calendar rule: " << boost::uuids::to_string(rule_.id);

    QPointer<CalendarRuleDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id_str = boost::uuids::to_string(rule_.id)]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_calendar_rule_request request;
        request.ids = {id_str};
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
            BOOST_LOG_SEV(lg(), info) << "Calendar Rule deleted successfully";
            emit self->statusMessage(QString("Calendar Rule '%1' deleted").arg(code));
            emit self->ruleDeleted(code);
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
