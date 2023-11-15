module Ifui.BulmaViews

import Ifui
import Ifui.Bulma
import Ifui.PureExpressions


export
interface View a where
  bulmaView : a -> Widget b

export
interface EditView a where
  bulmaEditView : a -> Widget a

export
View String where
  bulmaView = text

export
EditView String where
  bulmaEditView = textInputBulma

export
{s : String} -> EditView a  => EditView (Entry s a) where
  bulmaEditView x = MkEntry s <$> labeled s (bulmaEditView $ value x)

export
{ts : Vect n (String, Type)} -> AllI EditView ts =>
                EditView (Record ts) where
  bulmaEditView {ts = []} x = pure []
  bulmaEditView {ts = ((y, z) :: xs)} (x :: w) = (::) <$> bulmaEditView x <*> bulmaEditView w

mutual
  recEdits : (ts : Vect n (String, PTy)) -> Record (mapValues PTyType ts) -> Widget (Record (mapValues PTyType ts))
  recEdits [] x = pure []
  recEdits ((y, z) :: xs) (w :: ws) = (::) <$> (MkEntry y <$> (labeled y $ pTyTypeEditView z (value w))) <*> recEdits xs ws

  export
  pTyTypeEditView : (t : PTy) -> PTyType t -> Widget (PTyType t)
  pTyTypeEditView PString x = bulmaEditView x
  pTyTypeEditView PBool x = ?pTyTypeEditView_rhs_1
  pTyTypeEditView PUnit x = ?pTyTypeEditView_rhs_2
  pTyTypeEditView PInt x = ?pTyTypeEditView_rhs_3
  pTyTypeEditView (PList y) x = ?pTyTypeEditView_rhs_4
  pTyTypeEditView PNat x = ?pTyTypeEditView_rhs_5
  pTyTypeEditView PDouble x = ?pTyTypeEditView_rhs_6
  pTyTypeEditView (PTensor ks y) x = ?pTyTypeEditView_rhs_7
  pTyTypeEditView (PTuple y z) x = ?pTyTypeEditView_rhs_8
  pTyTypeEditView (PFun y z) x = ?pTyTypeEditView_rhs_9
  pTyTypeEditView (PRecord ts) x = recEdits (listToVect ts) x
  pTyTypeEditView (PTree xs) x = ?pTyTypeEditView_rhs_11
  pTyTypeEditView (PForall y) x = ?pTyTypeEditView_rhs_12
  pTyTypeEditView PPDF x = ?pTyTypeEditView_rhs_13


export
getFormBulma : EditView a => a -> Widget (Maybe a)
getFormBulma x = formBulma bulmaEditView x
