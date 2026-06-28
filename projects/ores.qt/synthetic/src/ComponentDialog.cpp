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
#include "ores.qt/ComponentDialog.hpp"
#include "ores.synthetic.api/messaging/gmm_component_protocol.hpp"
#include <QDialogButtonBox>
#include <QFormLayout>
#include <QFutureWatcher>
#include <QHBoxLayout>
#include <QLabel>
#include <QMessageBox>
#include <QPointer>
#include <QPushButton>
#include <QVBoxLayout>
#include <QtConcurrent>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <utility>

namespace ores::qt {

using namespace ores::logging;

ComponentDialog::ComponentDialog(ClientManager* cm,
                                 const QString& username,
                                 const boost::uuids::uuid& parentFxId,
                                 int nextIndex,
                                 QWidget* parent)
    : QDialog(parent)
    , clientManager_(cm)
    , username_(username)
    , isNew_(true) {

    component_.id = boost::uuids::random_generator()();
    component_.fx_spot_config_id = parentFxId;
    component_.party_id = clientManager_->currentPartyId();
    component_.component_index = nextIndex;
    component_.mean = 0.0;
    component_.stdev = 0.001;
    component_.weight = 0.0;

    BOOST_LOG_SEV(lg(), info) << "Opening component dialog for a new component "
                              << boost::uuids::to_string(component_.id) << " under fx pair "
                              << boost::uuids::to_string(parentFxId) << ".";
    buildUi();
}

ComponentDialog::ComponentDialog(ClientManager* cm,
                                 const QString& username,
                                 const synthetic::domain::gmm_component& existing,
                                 QWidget* parent)
    : QDialog(parent)
    , clientManager_(cm)
    , username_(username)
    , isNew_(false)
    , component_(existing) {

    BOOST_LOG_SEV(lg(), info) << "Opening component dialog editing component "
                              << boost::uuids::to_string(component_.id) << ".";
    buildUi();
}

void ComponentDialog::buildUi() {
    setWindowTitle(isNew_ ? tr("New Component") : tr("Edit Component"));
    setModal(true);

    auto* layout = new QVBoxLayout(this);

    auto* intro = new QLabel(
        tr("<p>Each component is one <b>bell curve</b> describing the random step the "
           "price takes every tick. Combine a few to shape how the rate behaves:</p>"
           "<ul>"
           "<li><b>Mean</b> — average drift per tick. Leave at <b>0</b> for no trend.</li>"
           "<li><b>Volatility (Stdev)</b> — typical size of each move; bigger = choppier. "
           "Try ~0.0003 (calm), 0.001 (normal), 0.004 (volatile).</li>"
           "<li><b>Weight</b> — how much this component contributes; weights are "
           "normalised across all components, so a single component with weight 1 is just "
           "one bell curve.</li>"
           "</ul>"
           "<p>Not sure? Pick a preset below.</p>"),
        this);
    intro->setWordWrap(true);
    intro->setTextFormat(Qt::RichText);
    layout->addWidget(intro);

    // Quick presets: fill mean=0, weight=1 and a sensible volatility so a
    // non-expert can get a working component in one click.
    auto* presetRow = new QHBoxLayout();
    presetRow->addWidget(new QLabel(tr("Quick preset:"), this));
    struct Preset {
        const char* label;
        double stdev;
    };
    for (const auto& p : {Preset{"Calm", 0.0003}, Preset{"Normal", 0.001},
                          Preset{"Volatile", 0.004}}) {
        auto* b = new QPushButton(tr(p.label), this);
        b->setToolTip(tr("Set drift 0, volatility %1, weight 1.").arg(p.stdev));
        const double sd = p.stdev;
        connect(b, &QPushButton::clicked, this, [this, sd]() {
            meanSpin_->setValue(0.0);
            stdevSpin_->setValue(sd);
            weightSpin_->setValue(1.0);
        });
        presetRow->addWidget(b);
    }
    presetRow->addStretch();
    layout->addLayout(presetRow);

    auto* form = new QFormLayout();

    indexSpin_ = new QSpinBox(this);
    indexSpin_->setRange(0, 100000);
    indexSpin_->setValue(component_.component_index);
    indexSpin_->setToolTip(tr("Ordering only; auto-incremented."));

    meanSpin_ = new QDoubleSpinBox(this);
    meanSpin_->setRange(-1e9, 1e9);
    meanSpin_->setDecimals(6);
    meanSpin_->setValue(component_.mean);
    meanSpin_->setToolTip(tr("Average per-tick drift; usually 0."));

    stdevSpin_ = new QDoubleSpinBox(this);
    stdevSpin_->setRange(0.0000001, 1e9);
    stdevSpin_->setDecimals(7);
    stdevSpin_->setValue(component_.stdev > 0 ? component_.stdev : 0.001);
    stdevSpin_->setToolTip(tr("Volatility per tick, e.g. 0.001; larger = choppier."));

    weightSpin_ = new QDoubleSpinBox(this);
    weightSpin_->setRange(0.0, 1e9);
    weightSpin_->setDecimals(6);
    weightSpin_->setValue(component_.weight);
    weightSpin_->setToolTip(
        tr("Relative proportion of this component; weights are normalised across all "
           "components."));

    form->addRow(tr("Component index"), indexSpin_);
    form->addRow(tr("Mean (drift)"), meanSpin_);
    form->addRow(tr("Volatility (stdev)"), stdevSpin_);
    form->addRow(tr("Weight (share)"), weightSpin_);
    layout->addLayout(form);

    auto* buttons = new QDialogButtonBox(QDialogButtonBox::Save | QDialogButtonBox::Cancel, this);
    layout->addWidget(buttons);
    connect(buttons, &QDialogButtonBox::accepted, this, &ComponentDialog::onSave);
    connect(buttons, &QDialogButtonBox::rejected, this, &QDialog::reject);
}

void ComponentDialog::onSave() {
    auto c = component_;
    c.component_index = indexSpin_->value();
    c.mean = meanSpin_->value();
    c.stdev = stdevSpin_->value();
    c.weight = weightSpin_->value();
    c.modified_by = username_.toStdString();
    c.change_reason_code = isNew_ ? "system.new_record" : "common.non_material_update";
    c.change_commentary = "Authored via Market Simulator";
    c.version = 0;

    const std::string id = boost::uuids::to_string(c.id);
    BOOST_LOG_SEV(lg(), info) << "Saving component " << id << " (new=" << isNew_ << ").";

    QPointer<ComponentDialog> self = this;
    auto* cm = clientManager_;

    auto task = [cm, c]() -> std::pair<bool, QString> {
        auto resp = cm->process_authenticated_request(
            synthetic::messaging::save_gmm_component_request::from(c));
        if (!resp)
            return {false, QString::fromStdString(resp.error())};
        if (!resp->success)
            return {false, QString::fromStdString(resp->message)};
        return {true, {}};
    };

    auto* watcher = new QFutureWatcher<std::pair<bool, QString>>(self);
    connect(watcher, &QFutureWatcher<std::pair<bool, QString>>::finished, self,
            [self, watcher, id]() {
                auto [ok, err] = watcher->result();
                watcher->deleteLater();
                if (!self)
                    return;
                if (!ok) {
                    BOOST_LOG_SEV(lg(), error)
                        << "Save failed for component " << id << ": " << err.toStdString();
                    QMessageBox::critical(self, self->tr("Save failed"), err);
                    return;
                }
                BOOST_LOG_SEV(lg(), info) << "Saved component " << id << ".";
                self->accept();
            });
    watcher->setFuture(QtConcurrent::run(task));
}

}
